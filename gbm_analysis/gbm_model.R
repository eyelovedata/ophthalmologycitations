setwd('C:\\Users\\veena\\Downloads\\ophthalmologycitations-master')
df_scopus_csv <- read.csv('dfscopus-2.csv')

# df cleanup from rmd file
library(tidyverse)
df_scopus_csv <-select(df_scopus_csv, -index, -authorlist, -authoridlist, -access.type)
df_scopus_csv <-filter(df_scopus_csv, !is.na(tc))

quantile(df_scopus_csv$tc, c(0.75))
df_scopus_csv$tcB<-ifelse(df_scopus_csv$tc>=20, 1, 0)

colnames(df_scopus_csv)[colSums(is.na(df_scopus_csv)) > 0]

df_scopus_csv$pagecount=replace_na(df_scopus_csv$pagecount, 1)
df_scopus_csv[is.na(df_scopus_csv)] = 0

df_train<-filter(df_scopus_csv, split==0)
df_dev<-filter(df_scopus_csv,split==1)
df_test <- filter(df_scopus_csv, split==2)

library(gbm)

LogLossBinary = function(actual, predicted, eps = 1e-15) {  
  predicted = pmin(pmax(predicted, eps), 1-eps)  
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

# gbm source
# https://www.datatechnotes.com/2018/03/classification-with-gradient-boosting.html

library(caret)
library(e1071)

# train with train set
cv.gbmModel = gbm(formula = df_train$tcB ~ . - df_train$tc - df_train$split - df_train$pagecount, data = df_train,
                  distribution = "bernoulli",
                  n.trees = 100,
                  shrinkage = .01,
                  n.minobsinnode = 20,
                  cv.folds = 10)

# functional approach to dev and test set
test_preds_with_gbm <- function(gbm_data, num_trees) {
  cv.gbm_predictions <- predict(object = cv.gbmModel,
                                newdata = gbm_data,
                                n.trees = num_trees,
                                type = 'response')
  
  print(LogLossBinary(gbm_data$tcB, cv.gbm_predictions))
  print(data.frame('Actual' = gbm_data$tcB, 'Predicted' = cv.gbm_predictions))
  
  cv.ifelse_preds <- ifelse(cv.gbm_predictions > 0.5, 1, 0)
  
  best_tree_for_prediction <- gbm.perf(cv.gbmModel)
  
  # https://stackoverflow.com/questions/38829646/confusion-matrix-of-bsttree-predictions-error-the-data-must-contain-some-leve
  cm = confusionMatrix(as.factor(gbm_data$tcB), as.factor(cv.ifelse_preds))
  print(cm)
}

test_preds_with_gbm(df_dev, 100)
test_preds_with_gbm(df_test, 100)
