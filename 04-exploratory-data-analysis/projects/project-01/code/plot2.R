################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 1
## Assignment: Course Project 1
## File: plot2.R
## Date: 2016-01-27
################################################################################

################################################################################
## This script is for creating plot 2.
################################################################################

## NOTE:
## The code assumes that the the subset of dataset is downloaded and saved
## in data/ directory with the name subset.csv by running preprocessing.R.

##################################################
## Load and manipulate data
##################################################

datafile <- "data/subset.csv"
df <- read.csv(datafile, sep=",", header=TRUE)
df$datetime <- as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")

##################################################
## Plot data
##################################################

png(file="plot2.png", width=480, height=480)

par(mfrow=c(1,1), mar=c(5,5,2,1), bg="transparent")

plot(df$datetime, df$Global_active_power, 
     type="l", 
     xlab="", ylab="Global Active Power (kilowatts)")

dev.off()

################################################################################