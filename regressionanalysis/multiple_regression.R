# use CSV with no NaN values to plot a multiple linear regression in R
# the CSV was generated from df.drop(df.columns.difference()), then df.to_csv in Python

setwd('C:\\Users\\veena\\Downloads')

all_scopus_droppedNaN <- read.csv('all_scopus_droppedNaN.csv')

# independent variables
author_number <- all_scopus_droppedNaN$author.number
page_count <- all_scopus_droppedNaN$paper.length.in.pages

q1 <- quantile(page_count, probs=0.25)
q3 <- quantile(page_count, probs=0.75)
outlier_step <- 1.5 * (q3-q1)

# I removed outliers from page count, and removed same indices from the other variables

# remove elements that are outliers by just removing them from the indices
page_count = page_count[which(page_count < (q3 + outlier_step))] # upper bound

# remove same elements in other variables for list similarity
author_number = author_number[which(page_count < (q3 + outlier_step))]

num_author_keywords <- all_scopus_droppedNaN$number.of.author.keywords
num_author_keywords = num_author_keywords[which(page_count < (q3 + outlier_step))]

num_index_keywords <- all_scopus_droppedNaN$number.of.index.keywords
num_index_keywords = num_index_keywords[which(page_count < (q3 + outlier_step))]

# dependent variable - times cited
times_cited <- all_scopus_droppedNaN$cited.by
times_cited = times_cited[which(page_count < (q3 + outlier_step))]

author_number_fit <- lm(times_cited ~ author_number, data=all_scopus_droppedNaN)
page_count_fit <- lm(times_cited ~ page_count, data=all_scopus_droppedNaN)
author_keywords_fit <- lm(times_cited ~ num_author_keywords, data=all_scopus_droppedNaN)
index_keywords_fit <- lm(times_cited ~ num_index_keywords, data=all_scopus_droppedNaN)

multiple_linReg_fit <- lm(times_cited ~ author_number + page_count + num_author_keywords + num_index_keywords, data=all_scopus_droppedNaN)
summary(multiple_linReg_fit)

var_vector <- c(author_number_fit, page_count_fit, author_keywords_fit, index_keywords_fit)

typeof(times_cited)
typeof(author_number)
# skip c - c is a constant reserved by R
nonlinReg <- nls(times_cited ~ author_number*a + page_count*b +
                 num_author_keywords*d + num_index_keywords*e, 
                 data = all_scopus_droppedNaN)

cor(times_cited, predict(nonlinReg)) # how close is the estimate? 1 = perfect model
plot(author_number + page_count + num_author_keywords + num_index_keywords, times_cited)
lines(author_number + page_count + num_author_keywords + num_index_keywords, predict(nonlinReg), col='blue')
rsq_allVariables <- cor(author_number + page_count + num_author_keywords + num_index_keywords, predict(nonlinReg)) ^ 2
rsq_allVariables

coefficients(nonlinReg)
confint(nonlinReg, level=0.95)
