################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 1
## File: week-01-quiz.R
################################################################################


################################################################################
# Question #1
################################################################################
# The American Community Survey distributes downloadable data
# about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho 
# using download.file() from here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
# and load the data into R. 
# The code book, describing the variable names is here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
# How many properties are worth $1,000,000 or more?

if (!file.exists("data")) {
    dir.create("data")
}

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url, destfile="data/housing-data.csv", method="curl")

housing_data <- read.csv("data/housing-data.csv", sep=",", header=TRUE)
subset <- housing_data[which(housing_data$VAL == 24), ]

print("Q1")
print(nrow(subset))


################################################################################
# Question #2
################################################################################
# Use the data you loaded from Question 1. 
# Consider the variable FES in the code book. 
# Which of the "tidy data" principles does this variable violate?

# Tidy data has one variable per column.


################################################################################
# Question #3
################################################################################
# Download the Excel spreadsheet on Natural Gas Aquisition Program here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx 
# Read rows 18-23 and columns 7-15 into R 
# and assign the result to a variable called:
#    dat 
# What is the value of:
#    sum(dat$Zip*dat$Ext,na.rm=T) 
# (original data source: http://catalog.data.gov/dataset/natural-gas-acquisition-program)

library(xlsx)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(url, destfile="data/gas-data.xlsx", method="curl")

gas_data <- read.xlsx("data/gas-data.xlsx", sheetIndex=1, header=TRUE)
col_index <- 7:15
row_index <- 18:23
dat <- read.xlsx("data/gas-data.xlsx", 
                 sheetIndex=1, colIndex=col_index, rowIndex=row_index)

print("Q3")
print(sum(dat$Zip*dat$Ext, na.rm=T))


################################################################################
# Question #4
################################################################################
# Read the XML data on Baltimore restaurants from here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml 
# How many restaurants have zipcode 21231?

library(XML)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(url, destfile="data/restaurant-data.xml", method="curl")

doc <- xmlTreeParse("data/restaurant-data.xml", useInternal=TRUE)
rootnode <- xmlRoot(doc)
zipcodes <- xpathSApply(rootnode, "//zipcode", xmlValue)
subset <- zipcodes[zipcodes == 21231]

print("Q4")
print(length(subset))


################################################################################
# Question #5
################################################################################
# The American Community Survey distributes downloadable data 
# about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho 
# using download.file() from here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv 
# using the fread() command load the data into an R object
# DT 
# Which of the following is the fastest way to calculate the average value 
# of the variable
# pwgtp15 
# broken down by sex using the data.table package?

library(data.table)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(url, destfile="data/idaho-data.csv", method="curl")
DT <- fread("data/idaho-data.csv")

print("Q5")
print(DT[, mean(pwgtp15), by=SEX])

###############################################################################