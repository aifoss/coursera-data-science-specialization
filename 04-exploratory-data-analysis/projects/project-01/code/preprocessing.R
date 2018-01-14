################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 1
## Assignment: Course Project 1
## File: preprocessing.R
## Date: 2016-01-26
################################################################################


################################################################################
## This script is for saving dataset as a preprocessing step for creating plots.
################################################################################

###############################################################
## Download and save dataset
###############################################################

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
datadir <- "data"
if (!file.exists(datadir)) {
    dir.create(datadir)
}
filename <- "project_data.zip"
filesep <- "/"
zipfile <- paste(datadir, filename, sep=filesep)
method <- "curl"

download.file(url, zipfile, method)
unzip(zipfile, exdir=datadir)
file.remove(zipfile)

###############################################################
## Save subset of data from dates in 2007-02-01 and 2007-02-02,
## removing NA values for Date/Time
###############################################################

## Read data

datafile <- paste(datadir, list.files(datadir)[1], sep=filesep)
data <- read.csv(datafile, sep=";", header=TRUE)

## Select subset

subset <- subset(data, Date=="1/2/2007" | Date=="2/2/2007")
subset <- subset(subset, Time!="?")
subset$Date <- as.Date(strptime(subset$Date, format="%d/%m/%Y"))

## Save subset

datafile <- "data/subset.csv"
write.csv(subset, file=datafile, row.names=FALSE)

################################################################################