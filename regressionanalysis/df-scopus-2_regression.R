setwd('C:\\Users\\veena\\Downloads\\ophthalmologycitations-master')
df_scopus_csv <- read.csv('dfscopus-2.csv')

# split by df row value
# https://rpubs.com/Mentors_Ubiqum/split_functions
df_scopus_split <- split(df_scopus_csv, df_scopus_csv$split)
train <- df_scopus_split$`0`
dev <- df_scopus_split$`1`
test <- df_scopus_split$`2`

# define *_tc, which is passed to the log_reg function below
train_tc <- train$tc
dev_tc <- dev$tc
test_tc <- test$tc

# get rid of row amount mismatches with NaN removal
# https://stackoverflow.com/questions/22016166/remove-or-find-nan-in-r/22016243

train <- na.omit(train)
dev <- na.omit(dev)
test <- na.omit(test)

train_tc <- na.omit(train_tc)
dev_tc <- na.omit(dev_tc)
test_tc <- na.omit(test_tc)

library(glmnet)

# glmnet requires a matrix input for x
# these models work for both linear and logistic regressions
model_train <- model.matrix(~ ., data=train)
model_dev_test <- model.matrix(~ ., data=dev)
model_final_test <- model.matrix(~ ., data=test)

# glmnet expects a matrix for x
# https://stackoverflow.com/questions/8457624/r-glmnet-list-object-cannot-be-coerced-to-type-double

# plot.matrix docs - https://cran.r-project.org/web/packages/plot.matrix/vignettes/plot.matrix.html
library(plot.matrix) # visualization of importance of each word
library(verification) # ROC

# ROC curve reference
# https://www.kaggle.com/captcalculator/logistic-regression-and-roc-curve-primer#The-ROC-Curve

log_reg <- function(train_model, test_model, train_tc_set, test_tc_set) { # use train and test in one function
  
  # logistic regression needs booleans - 1 if > 75th percentile, else 0
  train_tc_set_boolean <- train_tc_set > quantile(train_tc_set, c(0.75))
  
  # convert TRUE/FALSE to 1/0
  train_tc_set_numerical_boolean <- as.numeric(train_tc_set_boolean)
  print(train_tc_set_numerical_boolean)
  
  # create glm with cross validation
  # ensure that train matrix size = train bool size
  glm_fit_log <- cv.glmnet(train_model, train_tc_set_numerical_boolean[0:nrow(train_model)], alpha=1, family='binomial')
  print(summary(glm_fit_log))
  
  bestlam = glm_fit_log$lambda.min
  
  test_tc_set_boolean <- test_tc_set > quantile(test_tc_set, c(0.75))
  test_tc_set_numerical_boolean <- as.numeric(test_tc_set_boolean)
  
  # similarly, ensure that test pred size = test actual size = test matrix size
  test_tc_set_numerical_boolean <- test_tc_set_numerical_boolean[0:nrow(test_model)]
  
  print('actual')
  print(test_tc_set_numerical_boolean) # actual
  print(length(test_tc_set_numerical_boolean))
  
  test_pred = predict(glm_fit_log, newx = test_model, s=bestlam)
  
  test_pred_set_boolean <- test_pred > quantile(test_pred, c(0.75))
  test_pred_set_numerical_boolean <- as.numeric(test_pred_set_boolean) 
  
  test_pred_set_numerical_boolean <- test_pred_set_numerical_boolean[0:nrow(test_model)]
  
  print('predicted')
  print(test_pred_set_numerical_boolean) # predicted
  print(length(test_pred_set_numerical_boolean))
  
  # ROC finds validity of predictions
  
  if (isTRUE(all.equal(length(test_tc_set_numerical_boolean), length(test_pred_set_numerical_boolean)))) {
    roc.plot(test_tc_set_numerical_boolean, test_pred_set_numerical_boolean,
             threshold = seq(0, 1, 0.01))
  }
  else {
    print('Lengths of test TC and predicted test TC are not equal')
  }
}

# train and test with dev set
# then repeat with test set
log_reg(model_train, model_dev_test, train_tc, dev_tc)
log_reg(model_train, model_final_test, train_tc, test_tc)
