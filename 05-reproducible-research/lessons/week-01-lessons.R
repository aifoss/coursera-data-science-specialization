################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 5 Reproducible Research
## Week: Week 1
## File: week-01-lessons.R
## Date: 2016-01-31
################################################################################


################################################################################
## Structure of a Data Analysis
################################################################################

############################################################
## Data Subsampling
############################################################

## laod kernlab package
library(kernlab)

## load spam data
data(spam)

## perform subsampling
set.seed(3435)
train_indicator = rbinom(4601, size=1, prob=0.5)
print(table(train_indicator))
cat("\n")

spam_train = spam[train_indicator == 1, ]
spam_test = spam[train_indicator == 0, ]

############################################################
## Exploratory Data Analysis
############################################################

## names
print(names(spam_train))
cat("\n")

## head
print(head(spam_train))
cat("\n")

## classification summary
print(table(spam_train$type))
cat("\n")

## plot by type
plot(spam_train$capitalAve ~ spam_train$type)
plot(log10(spam_train$capitalAve+1) ~ spam_train$type)

## plot relationship between predictors
plot(log10(spam_train[, 1:4]+1))

## hierarchical clustering
h_cluster = hclust(dist(t(spam_train[, 1:57])))
plot(h_cluster)

h_cluster_updated = hclust(dist(t(log10(spam_train[, 1:55]+1))))
plot(h_cluster_updated)

############################################################
## Statistical Modeling/Prediction
############################################################

spam_train$typeNum = as.numeric(spam_train$type) - 1
cost_function = function(x, y) sum(x != (y > 0.5))
cv_error = rep(NA, 55)

library(boot)

for (i in 1:55) {
    lm_formula = reformulate(names(spam_train)[i], response="typeNum")
    glm_fit = glm(lm_formula, family="binomial", data=spam_train)
    cv_error[i] = cv.glm(spam_train, glm_fit, cost_function, 2)$delta[2]
    print(cv_error[i])
}
cat("\n")

## which predictor has minimum cross-validation error?
print(names(spam_train)[which.min(cv_error)])

##################################################
## Get a measure of uncertainty
##################################################

## use best model from the group
prediction_model = glm(typeNum ~ charDollar, family="binomial", data=spam_train)

## get predictions on the test set
prediction_test = predict(prediction_model, spam_test)
predicted_spam = rep("nonspam", dim(spam_test)[1])

## classify as 'spam' or those with prob > 0.5
predicted_spam[prediction_model$fitted > 0.5] = "spam"

## classification table
classification_table = table(predicted_spam, spam_test$type)
print(classification_table)
cat("\n")

## error rate
true_negative = classification_table[1]
false_positive = classification_table[2]
false_negative = classification_table[3]
true_positive = classification_table[4]
error_rate = (false_negative + false_positive) /
    (true_negative + false_negative + false_positive + true_positive)
print(error_rate)
cat("\n")

############################################################
## Interpreting Results
############################################################

############################################################
## Challenging Results
############################################################

############################################################
## Synthesizing Results
############################################################

################################################################################