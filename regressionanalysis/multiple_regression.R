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
# won't use test set TC until final model

fit <- lm(train_tc ~ . - train$X - train$id, data=train) # remove two "ID" variables
summary(fit)
# plot(fit)

# convert all dataFrame columns into factors
# except TC, X (which is id but zero-indexed) and id
# https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters

train %>% mutate_if(is.integer, as.factor) -> train

train$X <- as.integer(train$X)
train$id <- as.integer(train$id)
train$times.cited <- as.integer(train$times.cited)

dev$X <- as.integer(dev$X)
dev$id <- as.integer(dev$id)
dev$times.cited <- as.integer(dev$times.cited)

# re-initialize variables after conversion to factors
train_tc <- train$times.cited
dev_tc <- dev$times.cited

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

# get rid of row amount mismatches with NaN removal
# https://stackoverflow.com/questions/22016166/remove-or-find-nan-in-r/22016243
train_tc <- na.omit(train_tc)
dev_tc <- na.omit(dev_tc)

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
lin_reg(model, train_tc, FALSE)
lin_reg(model_dev_test, dev_tc, TRUE)

# same function as LinReg, but for logistic regression
log_reg <- function(log_model, tc_set, dev_bool) {
  # linear regression uses continuous scale for TC
  # but logistic regression needs booleans - 1 if > 75th percentile, else 0
  tc_set_boolean <- tc_set > quantile(tc_set, c(0.75))
  tc_set_boolean
  
  # convert TRUE/FALSE to 1/0
  tc_set_numerical_boolean <- as.numeric(tc_set_boolean)
  tc_set_numerical_boolean
  
  glm_fit_log <- cv.glmnet(log_model, tc_set_numerical_boolean, alpha=1, family='binomial')
  plot(glm_fit_log)
  
  bestlam = glm_fit_log$lambda.min
  bestlam
  
  i <- which(glm_fit_log$lambda == glm_fit_log$lambda.min)
  
  mse.min <- glm_fit_log$cvm[i]
  mse.min
  
  coef(glmnet(log_model, tc_set_numerical_boolean, alpha=1, lambda=bestlam))
  
  if (isTRUE(dev_bool)) {
    test_pred = predict(glm_fit_log, newx = log_model, s=bestlam)
    
    # logistic regression dev MSE
    mse <- mean((tc_set_numerical_boolean - exp(test_pred))^2)
    mse
    
  }
}
log_reg(model, train_tc, FALSE)
log_reg(model_dev_test, dev_tc, TRUE)
