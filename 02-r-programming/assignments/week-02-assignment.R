################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 2 R Programming
## Week: Week 2
## Assignment: Assignment 1
## File: week-02-assignment.R
## Date: 2016-01-09
################################################################################


################################################################################
## Part 1:
## Write a function named 'pollutantmean' that calculates the mean 
## of a pollutant (sulfate or nitrate) across a specified list of monitors. 
## The function 'pollutantmean' takes three arguments: 'directory', 
## 'pollutant', and 'id'. 
## Given a vector monitor ID numbers, 'pollutantmean' reads 
## that monitors' particulate matter data from the directory 
## specified in the 'directory' argument and returns the mean of the pollutant 
## across all of the monitors, ignoring any missing values coded as NA. 
################################################################################

pollutantmean <- function(directory, pollutant, id=1:332) {
    ## 'directory is a character vector of length 1
    ## indicating the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1
    ## indicating the name of the pollutant for which we will calculate
    ## the mean; either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    ## return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: do not round the result
    
    ## get list of files
    file_list <- list.files(directory, full.names=TRUE)
    ## read data from each file in the file list
    data_list <- lapply(file_list[id], read.csv)
    ## row-bind data from each file
    data <- do.call(rbind, data_list)
    ## compute/return mean, excluding NA values
    mean(data[, pollutant], na.rm=TRUE)
}


################################################################################
## Part 2:
## Write a function that reads a directory full of files and reports the number 
## of completely observed cases in each data file. The function should return 
## a data frame where the first column is the name of the file 
## and the second column is the number of complete cases.
################################################################################

complete <- function(directory, id=1:332) {
    ## 'directory' is a character vector of length 1
    ## indicating the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    ## return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the number of
    ## complete cases
    
    ## get list of files
    file_list <- list.files(directory, full.names=TRUE)
    ## create an empty dataframe 
    length <- length(id)
    result <- data.frame(nrow=length, ncol=2)
    colnames(result) <- c("id","nobs")
    ## set id list idx to 1
    idx <- 1
    
    ## iterate through id list
    while (idx <= length) {
        ## read data from corresponding file
        data <- read.csv(file_list[id[idx]])
        ## isolate complete cases
        complete_cases <- data[complete.cases(data), ]
        
        ## extract id and nobs
        m_id <- complete_cases[1, "ID"]
        nobs <- length(complete_cases[, "ID"])
        
        ## set id and nobs values in dataframe
        result[idx, "id"] <- m_id
        result[idx, "nobs"] <- nobs
        
        ## increment idx
        idx <- idx+1
    }
    
    ## return resulting dataframe
    result
}

################################################################################
## Part 3:
## Write a function that takes a directory of data files and a threshold
## for complete cases and calculates the correlation between sulfate and
## nitrate for monitor locations where the number of completely observed cases
## (on all variables) is greater than the threshold.
## The function should return a vector of correlations for the monitors 
## that meet the threshold requirement. If no monitors meet the threshold 
## requirement, then the function should return a numeric vector of length 0. 
################################################################################

## sample run (from parent directory):
## corr("data/specdata/", threshold=0.1)

corr <- function(directory, threshold=0) {
    ## 'directory is a character vector of length 1 indicating the location
    ## of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the number
    ## of completely observed observations (on all variables) required
    ## to compute the correlation between nitrate and sulfate; the default
    ## is 0
    
    ## return a numeric vector of correlations 
    ## NOTE: do not round the result
    
    ## get list of files
    file_list <- list.files(directory, full.names=TRUE)
    ## get number of files
    length <- length(file_list)
    ## create an empty vector
    result <- c()
    
    ## iterate through each file in fie list
    for (i in 1:length) {
        ## if file name does not end with ".csv", exit the loop
        file_name <- file_list[i]
        if (substr(file_name, nchar(file_name)-3, nchar(file_name)) != '.csv') {
            break
        }
        
        ## read data from current file
        data <- read.csv(file_list[i])
        ## check number of complete observations
        complete_cases <- data[complete.cases(data), ]
        num_complete_obs <- length(complete_cases[, "ID"])
        
        ## if num_complete_obs > threshold
        if (num_complete_obs >= threshold) {
            ## compute correlation between sulfate and nitrate
            correlation <- cor(complete_cases$sulfate, 
                               complete_cases$nitrate)
            ## append current result to the result vector
            result <- c(result, correlation)
        }
    }
    
    ## return resulting vector
    result
}


################################################################################
## Sample Runs
################################################################################

pollutantmean("data/specdata", "sulfate", 1:10)
pollutantmean("data/specdata", "nitrate", 70:72)
pollutantmean("data/specdata", "nitrate", 23)

complete("data/specdata", 1)
complete("data/specdata", c(2,4,8,10,12))
complete("data/specdata", 30:25)
complete("data/specdata", 3)

cr <- corr("data/specdata", 150)
head(cr)
summary(cr)
cr <- corr("data/specdata", 400)
head(cr)
summary(cr)
cr <- corr("data/specdata", 5000)
summary(cr)
length(cr)
cr <- corr("data/specdata")
summary(cr)
length(cr)

################################################################################