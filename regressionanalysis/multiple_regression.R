# bool_linReg.R, but cleaned up
setwd('C:\\Users\\veena\\Downloads')
df95_csv <- read.csv('df95_percent_withTC.csv')

library(dplyr)
# split data with dplyr
# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function

df95_csv$id <- 1:nrow(df95_csv)
train <- df95_csv %>% dplyr::sample_frac(.80)
total_test <- dplyr::anti_join(df95_csv, train, by = 'id')

dev <- total_test %>% dplyr::sample_frac(.50) # 50% of the remaining 20% - 10% overall
test <- dplyr::anti_join(total_test, dev, by = 'id')

train_tc <- train$times.cited # train set TC
dev_tc <- dev$times.cited # dev set TC
test_tc <- test$times.cited

fit <- lm(train_tc ~ . - train$X - train$id, data=train) # remove two "ID" variables
summary(fit)
# plot(fit)

# use class instead of typeof()
# https://stackoverflow.com/questions/35689019/typeof-returns-integer-for-something-that-is-clearly-a-factor
class(train$times.cited) # integer
class(train$child) # factor

# create glmnet model
library(glmnet)

# glmnet requires a matrix input for x
# these models work for both linear and logistic regressions
model <- model.matrix(~ ., data=train)
model_dev_test <- model.matrix(~ ., data=dev)
model_final_test <- model.matrix(~ ., data=test)

# get rid of row amount mismatches with NaN removal
# https://stackoverflow.com/questions/22016166/remove-or-find-nan-in-r/22016243
train_tc <- na.omit(train_tc)
dev_tc <- na.omit(dev_tc)
test_tc <- na.omit(test_tc)

# glmnet expects a matrix for x
# https://stackoverflow.com/questions/8457624/r-glmnet-list-object-cannot-be-coerced-to-type-double

# linear regression function, one call for train model/train TC, other call for dev
lin_reg <- function(lin_model, tc_set, dev_bool) {
  
  # create a general linear model for linReg, using parameters provided
  glm_fit <- cv.glmnet(lin_model, tc_set, alpha=1) # alpha=1 is lasso penalization
  plot(glm_fit)
  
  # minimize lambda, specify the amount that penalization should occur to optimize the model
  bestlam = glm_fit$lambda.min 
  bestlam
  
  # best lambda = min lambda
  i <- which(glm_fit$lambda == glm_fit$lambda.min)
  
  mse.min <- glm_fit$cvm[i]
  mse.min
  
  # print coefficients of the general linear model after penalization
  coef(glmnet(lin_model, tc_set, alpha=1, lambda=bestlam))
  
  # https://stackoverflow.com/questions/5681166/what-evaluates-to-true-false-in-r
  if (isTRUE(dev_bool)) { # if the set being called is the dev set
    # the dev set is the intermediate test set, use it to evaluate the model
    test_pred = predict(glm_fit, newx = lin_model, s=bestlam)
    
    # linear regression dev MSE - metric to test efficacy
    mse <- mean((tc_set - exp(test_pred))^2)
    mse
    
  }
}

# plot.matrix docs - https://cran.r-project.org/web/packages/plot.matrix/vignettes/plot.matrix.html
library(plot.matrix) # visualization of importance of each word
library(verification) # ROC
library(ggplot2)
library(pROC) # ROC

# ROC curve reference
# https://www.kaggle.com/captcalculator/logistic-regression-and-roc-curve-primer#The-ROC-Curve

log_reg <- function(train_model, test_model, tc_set) { # use train and test in one function
  
  model_df <- as.data.frame(train_model) # all factors, including x, id
  print(model_df)
  model_df_logistic <- model_df[3:10] # all logistic factors
  
  # remove factors with all zero - https://stackoverflow.com/questions/6632018/delete-all-columns-with-0-from-matrix
  model_df_logistic <- model_df_logistic[,colSums(model_df_logistic^2) !=0]
  print(model_df_logistic)
  
  model_tc <- model_df$times.cited # train set
  
  log_model_df <- as.data.frame(test_model) # test set
  log_model_tc <- log_model_df$times.cited
  
  # loop through colnames - https://stackoverflow.com/questions/49889403/loop-through-dataframe-column-names-r
  for (i in colnames(model_df_logistic)) {
    logit_fit <- glm(model_df_logistic[[i]] ~ model_tc, data=model_df_logistic) # create glm model for 0s and 1s graphs
    newdat <- data.frame(model_tc=seq(min(model_tc), max(model_tc), len=length(model_tc)))
    newdat$logistic <- predict(logit_fit, newdat, type='response')
    
    plot(model_tc, model_df_logistic[[i]], xlab='times cited', ylab=i, pch=16) # i = colname
    lines(model_df_logistic[[i]] ~ model_tc, data=newdat, col='green')
  }
  
  # logistic regression needs booleans - 1 if > 75th percentile, else 0
  tc_set_boolean <- tc_set > quantile(tc_set, c(0.75))
  
  # convert TRUE/FALSE to 1/0
  tc_set_numerical_boolean <- as.numeric(tc_set_boolean)
  
  # create glm with cross validation
  glm_fit_log <- cv.glmnet(test_model, tc_set_numerical_boolean, alpha=1, family='binomial')
  
  bestlam = glm_fit_log$lambda.min
  bestlam
  
  test_pred = predict(glm_fit_log, newx = test_model, s=bestlam)
  print(test_pred)
    
  # if test_pred < 0, the word is not apparent, bool = 0
  # if test_pred > 0, the word is present, bool = 1
    
  test_pred_numerical_boolean <- sign(test_pred)
  test_pred_numerical_boolean[test_pred_numerical_boolean < 0] <- 0
    
  print(test_pred_numerical_boolean) # predictions
  print(tc_set_numerical_boolean) # actual data
    
  # ROC finds validity of predictions
  roc.plot(tc_set_numerical_boolean, test_pred_numerical_boolean,
          threshold = seq(0, max(test_pred_numerical_boolean), 0.01),
  plot.thres = c(0.03, 0.05))
    
  # another ROC - https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
  plot(roc(tc_set_numerical_boolean, test_pred_numerical_boolean, direction='<',
          col='blue', lwd=3))
    
}

log_reg(model, model_dev_test, dev_tc)
log_reg(model, model_final_test, test_tc)
