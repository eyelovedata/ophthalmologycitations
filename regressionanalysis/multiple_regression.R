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
model_train <- model.matrix(~ ., data=train)
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
library(ggplot2)
library(pROC) # ROC

# ROC curve reference
# https://www.kaggle.com/captcalculator/logistic-regression-and-roc-curve-primer#The-ROC-Curve

log_reg <- function(train_model, test_model, train_tc_set, test_tc_set) { # use train and test in one function
  
  # logistic regression needs booleans - 1 if > 75th percentile, else 0
  train_tc_set_boolean <- train_tc_set > quantile(train_tc_set, c(0.75))
  
  # convert TRUE/FALSE to 1/0
  train_tc_set_numerical_boolean <- as.numeric(train_tc_set_boolean)
  
  # create glm with cross validation
  glm_fit_log <- cv.glmnet(train_model, train_tc_set_numerical_boolean, alpha=1, family='binomial')
  print(summary(glm_fit_log))
  
  bestlam = glm_fit_log$lambda.min
  
  test_tc_set_boolean <- test_tc_set > quantile(test_tc_set, c(0.75))
  test_tc_set_numerical_boolean <- as.numeric(test_tc_set_boolean)
  
  print('actual')
  print(test_tc_set_numerical_boolean) # actual
  print(length(test_tc_set_numerical_boolean))
  
  test_pred = predict(glm_fit_log, newx = test_model, s=bestlam)
  
  test_pred_set_boolean <- test_pred > quantile(test_pred, c(0.75))
  test_pred_set_numerical_boolean <- as.numeric(test_pred_set_boolean) 
  
  print('predicted')
  print(test_pred_set_numerical_boolean) # predicted
  print(length(test_pred_set_numerical_boolean))
  
  # ROC finds validity of predictions
  
  if (isTRUE(all.equal(length(test_tc_set_numerical_boolean), length(test_pred_set_numerical_boolean)))) {
  roc.plot(test_tc_set_numerical_boolean, test_pred_set_numerical_boolean,
          threshold = seq(0, max(test_pred_set_numerical_boolean), 0.01))
  }
  else {
    print('Lengths of test TC and predicted test TC are not equal')
  }
}

log_reg(model_train, model_dev_test, train_tc, dev_tc)
log_reg(model_train, model_final_test, train_tc, test_tc)
