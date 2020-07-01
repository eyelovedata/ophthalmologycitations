# use CSV with no NaN values to plot a multiple linear regression in R
# the CSV was generated from df.drop(df.columns.difference()), then df.to_csv in Python

setwd('C:\\Users\\veena\\Downloads')

all_scopus_droppedNaN <- read.csv('all_scopus_droppedNaN.csv')

# independent variables
author_number <- all_scopus_droppedNaN$author.number
num_author_keywords <- all_scopus_droppedNaN$number.of.author.keywords
num_index_keywords <- all_scopus_droppedNaN$number.of.index.keywords

# dependent variable - times cited
times_cited <- all_scopus_droppedNaN$cited.by

multiple_linReg_fit <- lm(times_cited ~ author_number + num_author_keywords + num_index_keywords,
                          data = all_scopus_droppedNaN)

summary(multiple_linReg_fit)

plot(multiple_linReg_fit)
abline(multiple_linReg_fit, col='blue')
rsq_multiple_linReg <- summary(multiple_linReg_fit)$r.squared
rsq_multiple_linReg # 0.0062

# skip c - c is a reserved constant in R
nonlinReg <- nls(times_cited ~ author_number*a +
                 num_author_keywords*b + num_index_keywords*d, 
                 data = all_scopus_droppedNaN)

summary(nonlinReg)

all_predictor_variables <- author_number + num_author_keywords + num_index_keywords

plot(all_predictor_variables, times_cited)
lines(all_predictor_variables, predict(nonlinReg), col='blue')

plot(all_predictor_variables[which(times_cited < 350)], times_cited[which(times_cited < 350)])
plot.window(xlim=c(0, 150), ylim=c(0, 350))
lines(all_predictor_variables, predict(nonlinReg), col='blue')

coefficients(nonlinReg)
confint(nonlinReg, level=0.95)
