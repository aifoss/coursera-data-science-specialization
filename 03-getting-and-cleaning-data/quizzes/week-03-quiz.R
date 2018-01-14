################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 3
## File: week-03-quiz.R
################################################################################


################################################################################
# Question #1
################################################################################
# The American Community Survey distributes downloadable data 
# about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho 
# using download.file() from here: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
# and load the data into R. The code book, describing the variable names is here: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
# Create a logical vector that identifies the households on greater than 10 acres 
# who sold more than $10,000 worth of agriculture products. 
# Assign that logical vector to the variable agricultureLogical. 
# Apply the which() function like this to identify the rows of the data frame 
# where the logical vector is TRUE. which(agricultureLogical) 
# What are the first 3 values that result?

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
filename <- "data/idaho.csv"
#download.file(url, destfile=filename, method="curl")

idaho_data <- read.csv(filename)

logical <- (idaho_data$ACR == 3 & idaho_data$AGS == 6)
head <- head(idaho_data[which(logical), 1:3], 3)

print("Q1")
print(head)
cat("\n")

# 125, 238, 262


################################################################################
# Question #2
################################################################################
# Using the jpeg package read in the following picture of your instructor into R 
# https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg 
# Use the parameter native=TRUE. 
# What are the 30th and 80th quantiles of the resulting data? 
# (some Linux systems may produce an answer 638 different for the 30th quantile)

#install.packages("jpeg")
library(jpeg)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
filename <- "data/jeff.jpg"
#download.file(url, destfile=filename, method="curl")

jpg_data <- readJPEG(filename, native=TRUE)
quant1 <- quantile(jpg_data, probs=seq(0,1, 0.3), na.rm=TRUE)
quant2 <- quantile(jpg_data, probs=seq(0,1, 0.4), na.rm=TRUE)

print("Q2")
print(quant1)
print(quant2)
cat("\n")

# -15259150 -10575416

################################################################################
# Question #3
################################################################################
# Load the Gross Domestic Product data for the 190 ranked countries 
# in this data set: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
# Load the educational data from this data set: 
#    https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
# Match the data based on the country shortcode. 
# How many of the IDs match? 
# Sort the data frame in descending order by GDP rank (so United States is last). 
# What is the 13th country in the resulting data frame? 
# Original data sources: 
#    http://data.worldbank.org/data-catalog/GDP-ranking-table 
#    http://data.worldbank.org/data-catalog/ed-stats

library(dplyr)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
filename <- "data/gdp.csv"
#download.file(url, destfile=filename, method="curl")

gdp_data_orig <- read.csv(filename, skip=4, header=TRUE, nrow=190)

gdp_data <- select(gdp_data_orig, 1:5, -3)
colnames(gdp_data) <- c("CountryCode", "Ranking", "Economy", "Millions USD")

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
filename <- "data/edu.csv"
#download.file(url, destfile=filename, method="curl")

edu_data_orig <- read.csv(filename, header=TRUE) 
#edu_data <- select(edu_data_orig, CountryCode, Long.Name)
edu_data <- edu_data_orig

merged_data <- merge(gdp_data, edu_data, 
                     by.x="CountryCode", by.y="CountryCode", all=FALSE)
merged_data$Ranking <- as.numeric(merged_data$Ranking)

#sorted_data <- merged_data[order(merged_data$Ranking, decreasing=TRUE), ]
sorted_data <- arrange(merged_data, desc(Ranking))
thirteenth <- sorted_data[13, "Long.Name"]

print("Q3")
print(dim(merged_data))
print(thirteenth)
cat("\n")

# 189 matches, 13th country is St. Kitts and Nevis


###############################################################################
# Question #4
###############################################################################
# What is the average GDP ranking for the "High income: OECD" 
# and "High income: nonOECD" group?

#oecd <- merged_data[which(merged_data$Income.Group == 'High income: OECD'), ]
#non_oecd <- merged_data[which(merged_data$Income.Group == 'High income: nonOECD'), ]

#mean_oecd_rank <- mean(oecd$Ranking)
#mean_non_oecd_rank <- mean(non_oecd$Ranking)

oecd <- filter(merged_data, Income.Group == 'High income: OECD')
non_oecd <- filter(merged_data, Income.Group == 'High income: nonOECD')

mean_oecd_rank <- summarise(oecd, mean(Ranking))
mean_non_oecd_rank <- summarise(non_oecd, mean(Ranking))

print("Q4")
print(mean_oecd_rank)
print(mean_non_oecd_rank)
cat("\n")

# 32.96667, 91.91304


###############################################################################
# Question #$
###############################################################################
# Cut the GDP ranking into 5 separate quantile groups. 
# Make a table versus Income.Group. 
# How many countries are Lower middle income 
# but among the 38 nations with highest GDP?

library(Hmisc)

merged_data$gdpGroups <- cut2(merged_data$Ranking, g=5)
table <- table(merged_data$gdpGroups, merged_data$Income.Group)

print("Q5")
print(table)
cat("\n")

# 5

###############################################################################