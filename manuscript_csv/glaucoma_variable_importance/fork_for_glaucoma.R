# this program is for finding variable importance for specific glaucoma keywords
# in R
# it uses dfscopus-2 ... manuscript.csv, same as find_glaucoma_subset.py

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

# the idea for the glaucoma fork is to take the glaucoma-specific words
  # then take the specific variable importances of those words

# find the "glaucoma" words
# retina, uveitis, oculoplastics, cornea, neuroophthalmology

# https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
df_glaucoma_subset <- select(df_scopus_csv,'cornea', 'retina', 'glaucoma_title', 'retinal_title', 'retina_title', 'glaucoma_abstract', 'retinal_abstract', 'retina_abstract', 'tcB')
df_glaucoma_subset

gbm_model_glaucoma_subset <- train(tcB ~ .,
                                   data = df_glaucoma_subset,
                                   distribution = 'bernoulli',
                                   n.trees = 300,
                                   interaction.depth = 4,
                                   cv.folds = 10)

train_gbm_imp_glaucoma_subset <- varImp(gbm_model_glaucoma_subset, scale=FALSE)
print(train_gbm_imp_glaucoma_subset)

plot(train_gbm_imp_glaucoma_subset, top = 5)
plot(train_gbm_imp_glaucoma_subset, top = 8)

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
# 
# train_gbm_imp <- varImp(gbm_model_after_training, scale=FALSE)
# print(train_gbm_imp)
# 
# plot(train_gbm_imp, top = 5)
# plot(train_gbm_imp, top = 10)
# plot(train_gbm_imp, top = 20)

