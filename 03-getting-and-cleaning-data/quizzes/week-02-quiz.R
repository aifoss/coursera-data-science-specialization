################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 2
## File: week-02-quiz.R
################################################################################


################################################################################
# Question #1
################################################################################
# Register an application with the Github API here 
# https://github.com/settings/applications.
# Access the API to get information on your instructors repositories
# (hint: this is the url you want 
# "https://api.github.com/users/jtleek/repos").
# Use this data to find the time that the datasharing repo was created.
# What time was it created?
# This tutorial may be useful
# (https://github.com/hadley/httr/blob/master/demo/oauth2-github.r).
# You may also need to run the code in the base R package
# and not R studio.

install.packages("httr")
install.packages("httpuv")
library(httr)
library(jsonlite)

# set OAuth endpoints
# http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# registre application at https://github.com/settings/applications
# insert client ID and secret
myapp <- oauth_app("github", 
                   "aa69c72873980c946581", 
                   secret="838a580e3ce9d97a2c6b28f2dc4cc1db0a66396a")

# get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
c <- content(req)

# find when the datasharing repo was created
json <- fromJSON(toJSON(c))

print("Q1")
print(json[json$name=="datasharing",]$created_at)
cat("\n")

################################################################################
# Question #2
################################################################################
# The sqldf package allows for execution of SQL commands on R data frames.
# We will use the sqldf package to practice the queries we might send
# with the dbSendQuery command in RMySQL.
# Download the American Community Survey data
# and load it into an R object called acs.
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv 
# Which of the following commands will select only the data
# for the probability weights pwgtp1 with ages less than 50?

# install.packages("sqldf")
library(sqldf)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(url, destfile="data/acs_data.csv", method="curl")

acs <- read.csv("data/acs_data.csv", sep=",", header=TRUE)

print("Q2")
print(sqldf("select pwgtp1 from acs where AGEP < 50"))
cat("\n")


################################################################################
# Question #3
################################################################################
# Using the same data frame you created in the previous problem,
# what is the equivalent function to unique(acs$AGEP)

print("Q3")
print(sqldf("select distinct AGEP from acs"))
cat("\n")


################################################################################
# Question #4
################################################################################
# How many characters are in the 10th, 20th, 30th and 100th lines of HTML
# from this page: 
# http://biostat.jhsph.edu/~jleek/contact.html 
# (Hint: the nchar() function in R may be helpful)

conn <- url("http://biostat.jhsph.edu/~jleek/contact.html")
html <- readLines(conn)
close(conn)

nchar10 <- nchar(html[10])
nchar20 <- nchar(html[20])
nchar30 <- nchar(html[30])
nchar100 <- nchar(html[100])

print("Q4")
print(c(nchar10, nchar20, nchar30, nchar100))
cat("\n")


################################################################################
# Question #5
################################################################################
# Read this data set into R and report the sum of the numbers in the fourth 
# of the nine columns. 
# https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for 
# Original source of the data: http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for 
# (Hint this is a fixed width file format)

filename <- "data/cloudFront.for"
if (!file.exists(filename)) {    
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"    
    download.file(url, destfile=filename, method="curl")
}

x <- read.fwf(file=filename,               
              skip=4,              
              widths=c(10, 9,4, 9,4, 9,4, 9,4))

print("Q5")
print(sum(x$V4))

###############################################################################