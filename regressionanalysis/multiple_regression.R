# 7/1/20

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

print(train)
print(total_test)

train_tc <- train$times.cited
fit <- lm(train_tc ~ . - train$X - train$id, data=train)
summary(fit)
#plot(fit)

# convert all dataFrame columns into factors
# except TC, X (which is id but zero-indexed) and id
# https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters

train %>% mutate_if(is.integer, as.factor) -> train

train$X <- as.integer(train$X)
train$id <- as.integer(train$id)
train$times.cited <- as.integer(train$times.cited)

# use class instead of typeof()
# https://stackoverflow.com/questions/35689019/typeof-returns-integer-for-something-that-is-clearly-a-factor
class(train$times.cited) # integer
class(train$child) # factor

library(glmnet)

model <- model.matrix(~ ., data=train)

# get rid of row amount mismatches with NaN removal
# https://stackoverflow.com/questions/22016166/remove-or-find-nan-in-r/22016243
train_tc <- na.omit(train_tc)

# glmnet expects a matrix for x
# https://stackoverflow.com/questions/8457624/r-glmnet-list-object-cannot-be-coerced-to-type-double
glm_fit_train <- cv.glmnet(model, train_tc, alpha=1)

plot(glm_fit_train)

bestlam = glm_fit_train$lambda.min
bestlam

i <- which(glm_fit_train$lambda == glm_fit_train$lambda.min)

mse.min <- glm_fit_train$cvm[i]
mse.min

coef(glmnet(model, train_tc, alpha=1, lambda=bestlam))

# run predictions on the test set
testpred = predict(glm_fit_train, newx = model, s=bestlam)

# without squaring, the absolute value error is 3.27 * 10^190
# R simply gives infinity for the MSE^2, but the code structure works out
test_mse <- mean((train_tc - exp(testpred))^2)
test_mse
