################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 1
## Assignment: Course Project 1
## File: plot4.R
## Date: 2016-01-27
################################################################################


################################################################################
## This script is for creating plot 4.
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

png(file="plot4.png", width=480, height=480)

par(mfcol=c(2,2), mar=c(5,5,2,1), bg="transparent")

## Plot 1
plot(df$datetime, df$Global_active_power, 
     type="l", 
     xlab="", ylab="Global Active Power")

## Plot 2
plot(df$datetime, df$Sub_metering_1, 
     type="l", 
     xlab="", ylab="Energy sub metering")

lines(df$datetime, df$Sub_metering_2, col="red")
lines(df$datetime, df$Sub_metering_3, col="blue")

legend("topright",
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col=c("black", "red", "blue"), 
       lty=1,
       bty="n")

## Plot 3
with(df, plot(datetime, Voltage, type="l"))

## Plot 4
with(df, plot(datetime, Global_reactive_power, type="l"))

dev.off()

################################################################################