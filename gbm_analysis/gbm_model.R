setwd('C:\\Users\\veena\\Desktop\\ophthalmologycitations-master\\ophthalmologycitations-master')
df_scopus_csv <- read.csv('dfscopus-2_with_nlp_title_and_abstract.csv')

# df cleanup from rmd file
library(tidyverse)
df_scopus_csv <-select(df_scopus_csv, -index, -authorlist, -authoridlist, -access.type)
df_scopus_csv <-filter(df_scopus_csv, !is.na(tc))

quantile(df_scopus_csv$tc, c(0.75)) # 20
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
library(pROC)
library(precrec)

# define number of trees - keep consistent with train and dev/test
num_trees_var <- 300

# create a tuneGrid to provide vectors to caret trainControl()
gbmGrid <- expand.grid(interaction.depth = c(1, 2, 3, 4, 5, 6, 7, 8),
                        n.trees = seq(50, 500, 50),
                        shrinkage = seq(.001, .1, .01),
                        n.minobsinnode = c(10))

# train with train set
# use caret library to create trmodel, then caret train() function
fit_control <- caret::trainControl(## 10-fold cv
  method = 'repeatedcv',
  number = 10,
  repeats = 10,
  selectionFunction = 'best',
  savePredictions = TRUE)

#set.seed(1) # set seed for consistency
#caret_gbm_fit <- caret::train(as.factor(tcB) ~ . -tc -split -pagecount,
 #                             data = df_train,
  #                            method = 'gbm',
   #                           distribution = 'bernoulli',
    #                          trControl = fit_control,
     #                         tuneGrid = gbmGrid,
                              # gbm passes this parameter
      #                        verbose = FALSE)

# use caret_gbm_fit final values to create the optimal train gbm model
optimal_gbm_train_model <- gbm(tcB ~ ., 
                               data=select(df_train, -tc, -split, -pagecount),
                               distribution = "bernoulli",
                               n.trees = num_trees_var,
                               shrinkage = 0.031,
                               n.minobsinnode = 10,
                               interaction.depth = 4,
                               cv.folds = 10)

#print(caret_gbm_fit)
#plot(caret_gbm_fit)

print(optimal_gbm_train_model)

# functional approach to dev and test set
test_preds_with_gbm <- function(gbm_data, num_trees) {
  cv.gbm_predictions <- predict.gbm(object = optimal_gbm_train_model,
                                    newdata = gbm_data,
                                    n.trees = num_trees,
                                    type = 'response')
  
  print(LogLossBinary(gbm_data$tcB, cv.gbm_predictions))
  print(data.frame('Actual' = gbm_data$tcB, 'Predicted' = cv.gbm_predictions))
  
  best_tree_for_prediction <- gbm.perf(optimal_gbm_train_model, method='cv')
  print(best_tree_for_prediction)
  summary(optimal_gbm_train_model)
  
  cv.ifelse_preds <- ifelse(cv.gbm_predictions > quantile(cv.gbm_predictions, c(0.75)), 1, 0)
  
  print(table(gbm_data$tcB))
  print(table(cv.ifelse_preds))
  
  roc_graph <- roc(gbm_data$tcB, cv.gbm_predictions,
                   ci=TRUE, ci.alpha=0.95, plot=TRUE,
                   print.auc=TRUE, legacy.axes=TRUE)
  
  sens.ci <- ci.se(roc_graph)
  plot(sens.ci, type="shape", col="lightblue")
  
  auc_val <- pROC::auc(roc_graph)
  print(auc_val)
  
  precrec_obj <- precrec::evalmod(scores = cv.gbm_predictions, labels = gbm_data$tcB)
  autoplot(precrec_obj) 
}

#test_preds_with_gbm(df_dev, num_trees_var)
test_preds_with_gbm(df_test, num_trees_var)
