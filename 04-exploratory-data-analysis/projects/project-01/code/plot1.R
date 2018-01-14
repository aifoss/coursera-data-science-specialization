################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: Exploratory Data Analysis
## Week: Week 1
## Assignment: Course Project 1
## File: plot1.R
## Date: 2016-01-27
################################################################################

################################################################################
## This script is for creating plot 1.
################################################################################

## NOTE:
## The code assumes that the the subset of dataset is downloaded and saved
## in data/ directory with the name subset.csv by running preprocessing.R.

##################################################
## Load data
##################################################

datafile <- "data/subset.csv"
df <- read.csv(datafile, sep=",", header=TRUE)

##################################################
## Plot data
##################################################

png(file="plot1.png", width=480, height=480)

par(mfrow=c(1,1), mar=c(5,5,2,1), bg="transparent")

hist(df$Global_active_power, 
     col="red", 
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)")

dev.off()

################################################################################