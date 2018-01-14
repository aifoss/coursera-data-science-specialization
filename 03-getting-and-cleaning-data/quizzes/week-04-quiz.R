################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 4
## File: week-04-quiz.R
################################################################################


################################################################################
# Question #1
################################################################################
# The American Community Survey distributes downloadable data 
#    about United States communities. 
#    Download the 2006 microdata survey about housing for the state of Idaho
#    using download.file() from here: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
# and load the data into R. The code book, describing the variable names is here: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
# Apply strsplit() to split all the names of the data frame 
#    on the characters "wgtp". 
#    What is the value of the 123 element of the resulting list?

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
file <- "data/idaho.csv"
method <- "curl"
#download.file(url, destfile=file, method=method)

idaho_data <- read.csv(file)
names <- names(idaho_data)
split_names <- strsplit(names, "wgtp")
elem_123 <- split_names[[123]]

print("Q1")
print(elem_123)
cat("\n")


################################################################################
# Question #2
################################################################################
# Load the Gross Domestic Product data for the 190 ranked countries 
#    in this data set: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
# Remove the commas from the GDP numbers in millions of dollars and average them. 
# What is the average? 
# Original data sources: http://data.worldbank.org/data-catalog/GDP-ranking-table

library(stringr)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
file <- "data/gdp.csv"
#download.file(url, destfile=file, method=method)

gdp_data <- read.csv(file, sep=",", header=FALSE, skip=5, nrows=190)
gdp_data <- select(gdp_data, 1:2, 4:5)
names(gdp_data) <- c("CountryCode","Ranking","countryNames","MillionsInUSD")

gdp_figures <- gdp_data[, 4]
gdp_figures <- gsub(",", "", gdp_figures)
gdp_figures <- str_trim(gdp_figures)
ave <- mean(as.numeric(gdp_figures))

print("Q2")
print(ave)
cat("\n")


################################################################################
# Question #3
################################################################################
# In the data set from Question 2 what is a regular expression 
#    that would allow you to count the number of countries 
#    whose name begins with "United"? 
# Assume that the variable with the country names in it is named countryNames. 
# How many countries begin with United?

match <- grep("^United", gdp_data$countryNames, value=TRUE)
count <- length(match)

print("Q3")
print("grep(\"^United\", countryNames)")
print(count)
cat("\n")


################################################################################
# Question #4
################################################################################
# Load the Gross Domestic Product data for the 190 ranked countries 
#    in this data set: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
# Load the educational data from this data set: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
# Match the data based on the country shortcode. Of the countries 
#    for which the end of the fiscal year is available, how many end in June? 
# Original data sources: 
#    http://data.worldbank.org/data-catalog/GDP-ranking-table 
#    http://data.worldbank.org/data-catalog/ed-stats

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
file <- "data/edu.csv"
#download.file(url, destfile=file, method=method)

edu_data <- read.csv(file, sep=",", header=TRUE)
merged_data <- merge(gdp_data, edu_data, by="CountryCode", all=FALSE)

match <- grep("^Fiscal year end: June", merged_data$Special.Notes, value=TRUE)
count <- length(match)

print("Q4")
print(count)
cat("\n")


###############################################################################
# Question #5
###############################################################################
# You can use the quantmod (http://www.quantmod.com/) package 
#    to get historical stock prices for publicly traded companies 
#    on the NASDAQ and NYSE. 
# Use the following code to download data on Amazon's stock price 
#    and get the times the data was sampled.
# library(quantmod)
# amzn = getSymbols("AMZN",auto.assign=FALSE)
# sampleTimes = index(amzn) 
# How many values were collected in 2012? 
# How many values were collected on Mondays in 2012?

#install.packages("quantmod")
library(quantmod)
library(lubridate)

amzn <- getSymbols("AMZN", auto.assign=FALSE)
sample_times <- index(amzn)

year_match <- grep("^2012-", sample_times, value=TRUE)
year_match_count <- length(year_match)

wdays <- wday(year_match, label=TRUE)
wday_match <- wdays[wdays == "Mon"]
wday_match_count <- length(wday_match)

print("Q5")
print(year_match_count)
print(wday_match_count)
cat("\n")

###############################################################################