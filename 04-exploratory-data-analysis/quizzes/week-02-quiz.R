################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 2
## File: week-02-quiz.R
## Date: 2016-02-01
################################################################################


################################################################################
## Q2
## What is produced by the following code?
################################################################################

library(nlme)
library(lattice)
p <- xyplot(weight ~ Time | Diet, BodyWeight)

print("Q2")
print(p)
cat("\n")

################################################################################
## Q7
## Load the airquality dataset from the dataset package in R.
## I am interested in examining how the relationship between ozone and wind
## speed varies across each month.
## What would be the appropriate code to visualize that using ggplot2?
################################################################################

library(ggplot2)
library(datasets)
data(airquality)

airquality = transform(airquality, Month = factor(Month))
p <- qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

print("Q7")
print(p)
cat("\n")

################################################################################
## Q9
## When I run the following code I get an error:
## library(ggplot2)
## g <- ggplot(movies, aes(votes, rating))
## print(g)
## What's the problem?
################################################################################

library(ggplot2)
library(datasets)
data(movies)
g <- ggplot(movies, aes(votes, rating))
print(g)

################################################################################