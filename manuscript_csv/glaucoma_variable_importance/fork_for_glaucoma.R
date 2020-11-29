# this program is for finding variable importance for specific glaucoma keywords
# in R

# up until "glaucoma_subset" var, this is the same file
# as gbm_model_manuscript.R

# contains gbm model, nltk+rake analysis, and nltk tokenized keywords from titles and abstracts

setwd('C:\\Users\\Veena\\Downloads')
df_scopus_csv <- read.csv('dfscopus-2_with_title_and_abstract_manuscript.csv')

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

# no need to further tune the hyperparameters - the optimal values from the grid are in the model below

# https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame

# these are the 7 "glaucoma" columns
# ik_glaucoma, ik_open angle glaucoma, ak_glaucoma, journal of glaucoma
# glaucoma_title, glaucoma_abstract

# the goal is to take the glaucoma columns which are selectable by R
# i.e. the 4 following columns
# and then run the previously defined training set on these 4 columns as a test set

#df_glaucoma_test_set <- select(df_test, 'ik_glaucoma',
#                              'ak_glaucoma', 'glaucoma_title', 'glaucoma_abstract', 'tcB', 'year')

glaucoma_boolean <- dplyr::mutate(df_test, 
                                  glaucoma_indicator=ik_glaucoma + 
                                    ak_glaucoma + glaucoma_title + glaucoma_abstract)

retina_boolean <- dplyr::mutate(df_test,
                                retina_indicator=ik_retina + ak_retina +
                                  retina_title + retina_abstract)

cornea_boolean <- dplyr::mutate(df_test,
                                cornea_indicator=ik_cornea + ak_cornea + cornea)

var_imp_related_fields_boolean <- dplyr::mutate(df_test,
                                all_related_fields_indicator = ik_glaucoma +
                                  ak_glaucoma + glaucoma_title + glaucoma_abstract +
                                  ik_retina + ak_retina +
                                  retina_title + retina_abstract +
                                  ik_cornea + ak_cornea + cornea +
                                  ik_plastic + ik_uveitis + ik_neurology + 
                                  ik_cornea + ak_cornea + cornea
                                  )

df_glaucoma_test_set <- filter(glaucoma_boolean, glaucoma_indicator > 0)

df_retina_test_set <- filter(retina_boolean, retina_indicator > 0)

df_cornea_test_set <- filter(cornea_boolean, cornea_indicator > 0)

df_related_fields_test_set <- filter(var_imp_related_fields_boolean, all_related_fields_indicator > 0)

# other, non-glaucoma and retina words
# ik_plastic - oculoplastics
# ik_uveitis - uveitis
# ik_neurology, journal of neuro-ophthalmology - neuro
# ik_cornea, ak_cornea, cornea - cornea





print(df_glaucoma_test_set)
print(df_retina_test_set)
print(df_cornea_test_set)
print(df_related_fields_test_set)

write.csv(df_cornea_test_set, 'C:\\Users\\veena\\Desktop\\cornea_test_set.csv')

summary(glaucoma_boolean)
summary(retina_boolean)
summary(cornea_boolean)
summary(var_imp_related_fields_boolean)

new_glaucoma_test_set <- read.csv('glaucoma_test_set.csv')

# https://stackoverflow.com/questions/4131338/is-it-possible-to-have-a-multi-line-comments-in-r

# find variable importance with varImp
# gbm_model_after_training <- train(tcB ~ .,
#                                   data = select(df_train, -tc, -split, -X),
#                                   distribution = 'bernoulli',
#                                   n.trees = 300,
#                                   shrinkage = 0.031,
#                                   n.minobsinnode = 10,
#                                   interaction.depth = 4,
#                                   cv.folds = 10)

# train_gbm_imp <- varImp(gbm_model_after_training, scale=FALSE)
# print(train_gbm_imp)

# write.csv(train_gbm_imp$importance, "C:\\Users\\Veena\\Desktop\\train_gbm_imp.csv")
# 
# plot(train_gbm_imp, top = 5)
# plot(train_gbm_imp, top = 10)
# plot(train_gbm_imp, top = 20)
# plot(train_gbm_imp, top = 100)

# plot the ROC curve, using the old train set and the new test set
# optimal_gbm_train_model <- gbm(tcB ~ ., 
#                                data=select(df_train, -tc, -split, -X),
#                                distribution = "bernoulli",
#                                n.trees = 300,
#                                shrinkage = 0.031,
#                                n.minobsinnode = 10,
#                                interaction.depth = 4,
#                                cv.folds = 10)

#print(optimal_gbm_train_model)

# functional approach to dev and test set
test_preds_with_gbm <- function(gbm_data) {
  cv.gbm_predictions <- predict.gbm(object = optimal_gbm_train_model,
                                    newdata = gbm_data,
                                    n.trees = 300,
                                    type = 'response')
  
  #print(LogLossBinary(gbm_data$tcB, cv.gbm_predictions))
  #print('actual cv model values')
  #values = data.frame('Actual' = gbm_data$tcB, 'Predicted' = cv.gbm_predictions)
  #print(values)
  #write.csv(values, "C:\\Users\\Veena\\Desktop\\values_probabilities.csv")
  
  best_tree_for_prediction <- gbm.perf(optimal_gbm_train_model, method='cv')
  print(best_tree_for_prediction)
  summary(optimal_gbm_train_model)
  
  cv.ifelse_preds <- ifelse(cv.gbm_predictions > quantile(cv.gbm_predictions, c(0.75)), 1, 0)
  
  #print('cv if else predictions')
  #quartile_values = data.frame('Actual' = gbm_data$tcB, 'Predicted' = cv.ifelse_preds)
  #print(quartile_values)
  #write.csv(quartile_values, "C:\\Users\\Veena\\Desktop\\quartile_values_probabilities.csv")
  
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

test_preds_with_gbm(df_related_fields_test_set)
