################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 2 R Programming
## Week: Week 4
## Assignment: Assignment 3
## File: week-04-assignment.R
## Date: 2016-01-09
################################################################################


outcome_file <- "data/hospitaldata/outcome-of-care-measures.csv"

run <- function(state, outcome, num) {
    plot_mortality_rate()
    best(state, outcome)
    rank_hospital(state, outcome, num)
    rank_all(outcome, num)
}


################################################################################
## 1. Plot the 30-day mortality rates for heart attack
################################################################################
## Make a simple histogram of the 30-day death rates from heart attack 
## (column 11 in the outcome dataset)
################################################################################

plot_mortality_rate <- function() {
    print("***************************************************")
    print("1: plot_mortality_rate()")
    
    ## read outcome data
    outcome <- read.csv(outcome_file, colClasses="character")
    
    print(paste("nrow =", nrow(outcome)))
    print(paste("ncol =", ncol(outcome)))
    
    ## convert mortality rate column to numeric vector
    mortality_rate_col_idx <- 11
    mortality_rate_column <- as.numeric(outcome[, mortality_rate_col_idx])
    
    # plot mortality rate histogram
    hist(mortality_rate_column,
         main="Histogram of 30-Day Mortality Rates", 
         xlab="Mortality Rate")
    
    print("***************************************************")
}


################################################################################
## 2. Find the best hopistal in the state
################################################################################
## Write a function called best that take two arguments: 
## the 2-character abbreviated name of a state and an outcome name. 
## The function reads the outcome-of-care-measures.csv file and returns
## a character vector with the name of the hospital that has the best
## (i.e. lowest) 30-day mortality for the specified outcome in that state.
##
## The hospital name is the name provided in the Hospital.Name variable.
## The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. 
## Hospitals that do not have data on a particular outcome should be excluded 
## from the set of hospitals when deciding the rankings.
##
## Handling ties: If there is a tie for the best hospital for a given outcome, 
## then the hospital names should be sorted in alphabetical order 
## and the first hospital in that set should be chosen 
## (i.e. if hospitals “b”, “c”, and “f” are tied for best, 
## then hospital “b” should be returned).
##
## The function should check the validity of its arguments. 
## If an invalid state value is passed to best, the function should throw 
## an error via the stop function with the exact message “invalid state”. 
## If an invalid outcome value is passed to best, the function should throw 
## an error via the stop function with the exact message “invalid outcome”.
################################################################################

best <- function(state, outcome) {
    cat("\n")
    print("***************************************************")
    print(paste("2: best()", state, outcome))
    
    ## read outcome data
    data <- read.csv(outcome_file, colClasses="character");
    
    ## get/set state names and outcome names/columns
    state_name_col_idx <- 7
    state_names <- unique(data[, state_name_col_idx])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    columns <- c(11, 17, 23); 
    
    ## check that state and outcome arguments are valid
    if (!(state %in% state_names)) {
        stop("invalid state")
    }
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    
    ## get hopistal name and outcome column indices
    hospital_name_col_idx <- 2
    outcome_index <- match(outcome, outcomes);
    outcome_col_idx <- columns[outcome_index];    
    
    ## convert outcome column to numeric vectro
    data[, outcome_col_idx] <- as.numeric(data[, outcome_col_idx])
    
    ## get subset of data that matches state name
    state_subset <- data[which(data[, state_name_col_idx] == state), ]
    
    ## get subset of state data with hospital name and outcome columns only
    outcome_subset <- state_subset[, c(hospital_name_col_idx,
                                       outcome_col_idx)]
    
    ## exclude data with NA outcome values
    non_na_subset <- outcome_subset[
        which(outcome_subset[, 2] != "Not Available"), ]
    
    ## sort subset by increasing order of mortality rate and hospital name
    sorted_subset <- non_na_subset[order(
        non_na_subset[, 2],
        non_na_subset[, 1]), ]
    
    ## return name of hospital with lowest mortality rate
    best_hospital <- sorted_subset[1, 1]
    print(best_hospital)
    
    print("***************************************************")
}


################################################################################
## 3. Rank hospitals by outcome in a state
################################################################################
## Write a function called rankhopistal that takes three arguments:
## the 2-character abbreviated name of a state (state), an outcome (outcome),
## and the ranking of a hopistal in that state for that outcome (num).
##
## The function reads the outcome-of-care-measures.csv file and returns
## a character vector with the name of the hospital that has the ranking
## specified by the num argument.
##
## The num argument can take values “best”, “worst”, or an integer indicating 
## the ranking (smaller numbers are better). If the number given by num 
## is larger than the number of hospitals in that state, then the function 
## should return NA. Hospitals that do not have data on a particular outcome 
## should be excluded from the set of hospitals when deciding the rankings.
##
## Handling ties: It may occur that multiple hospitals have the same 
## 30-day mortality rate for a given cause of death. In those cases 
## ties should be broken by using the hospital name.
##
## The function should check the validity of its arguments. If an invalid state 
## value is passed, the function should throw an error via the stop function 
## with the exact message “invalid state”. If an invalid outcome value is passed, 
## the function should throw an error via the stop function with the exact
## message “invalid outcome”.
################################################################################

rank_hospital <- function(state, outcome, num = "best") {
    cat("\n")
    print("***************************************************")
    print(paste("3: rank_hospital()", state, outcome, num))
    
    ## read outcome data
    data <- read.csv(outcome_file, colClasses="character");
    
    ## get/set state names and outcome names/columns
    state_name_col_idx <- 7
    state_names <- unique(data[, state_name_col_idx])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    columns <- c(11, 17, 23); 
    
    ## check that state and outcome arguments are valid
    if (!(state %in% state_names)) {
        stop("invalid state")
    }
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    
    ## get hopistal name and outcome column indices
    hospital_name_col_idx <- 2
    outcome_index <- match(outcome, outcomes);
    outcome_col_idx <- columns[outcome_index];    
    
    ## convert outcome column to numeric vectro
    data[, outcome_col_idx] <- as.numeric(data[, outcome_col_idx])
    
    ## get subset of data that matches state name
    state_subset <- data[which(data[, state_name_col_idx] == state), ]
    
    ## get subset of state data with hospital name and outcome columns only
    outcome_subset <- state_subset[, c(hospital_name_col_idx,
                                       outcome_col_idx)]
    
    ## exclude data with NA outcome values
    non_na_subset <- outcome_subset[
        which(outcome_subset[, 2] != "Not Available"), ]
    
    ## sort subset by increasing order of mortality rate and hospital name
    sorted_subset <- non_na_subset[order(
        non_na_subset[, 2],
        non_na_subset[, 1]), ]
    
    ## return name of hospital with specified ranking
    last_row <- nrow(sorted_subset)
    ranked_hospital <- NULL
    
    if (num == "best") {
        ranked_hospital <- sorted_subset[1, 1]
    } else if (num == "worst") {
        ranked_hospital <- sorted_subset[last_row, 1]
    } else if (num < 1 || num > last_row) {
        ranked_hospital <- "NA"
    } else {
        ranked_hospital <- sorted_subset[num, 1]
    }
    
    print(ranked_hospital)
    
    print("***************************************************")
}


################################################################################
## 4. Rank hospitals in all states
################################################################################
## Write a function called rankall that takes two arguments: an outcome name
## (outcome) and a hospital ranking (num). The function reads the outcome-of-
## care-measures.csv file and returns a 2-column data frame containing 
## the hospital in each state that has the ranking specified in num.
##
## The first column in the data frame is named hopistal, which contains
## the hospital name, and the second column is named state, which contains
## the 2-character abbreviation for the state name. Hospitals that do not have
## data on a particular outcome should be excluded from the set of hospitals
## when deciding the rankings.
## 
## Handling ties: The rankall function should handle ties in the 30-day
## mortality rates in the same way that the rankhospital function handles ties.
##
## The function should check the validity of its arguments. If an invalid outcome 
## value is passed to rankall, the function should throw an error via the stop 
## function with the exact message “invalid outcome”. The num variable can take 
## values “best”, “worst”, or an integer indicating the ranking (smaller numbers 
## are better). If the number given by num is larger than the number of hospitals 
## in that state, then the function should return NA.
################################################################################

rank_all <- function(outcome, num = "best") {
    cat("\n")
    print("***************************************************")
    print(paste("4: rank_all()", outcome, num))
    
    ## read outcome data
    data <- read.csv(outcome_file, colClasses="character");
    
    ## get/set state names and outcome names/columns
    state_name_col_idx <- 7
    state_names <- unique(data[, state_name_col_idx])
    num_states <- length(state_names)
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    columns <- c(11, 17, 23); 
    
    ## check that outcome argument is valid
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    
    ## get hopistal name and outcome column indices
    hospital_name_col_idx <- 2
    outcome_index <- match(outcome, outcomes);
    outcome_col_idx <- columns[outcome_index];    
    
    ## convert outcome column to numeric vectro
    data[, outcome_col_idx] <- as.numeric(data[, outcome_col_idx])
    
    ## create an empty data frame to store result
    ##result <- data.frame(hospital=character(), state=character())
    result <- data.frame(matrix(nrow=num_states, ncol=2))
    colnames(result) <- c("hospital", "state")
    
    ## for each state, find the hospital of the given rank
    for (i in 1:num_states) {
        ## get subset of data that matches state name
        state <- state_names[i]
        state_subset <- data[which(data[, state_name_col_idx] == state), ]
        
        ## get subset of state data with hospital name and outcome columns only
        outcome_subset <- state_subset[, c(hospital_name_col_idx,
                                           outcome_col_idx)]
        
        ## exclude data with NA outcome values
        non_na_subset <- outcome_subset[
            which(outcome_subset[, 2] != "Not Available"), ]
        
        ## sort subset by increasing order of mortality rate and hospital name
        sorted_subset <- non_na_subset[order(
            non_na_subset[, 2],
            non_na_subset[, 1]), ]
        
        ## return name of hospital with specified ranking
        last_row <- nrow(sorted_subset)
        hospital <- NULL
        
        if (num == "best") {
            hospital <- sorted_subset[1, 1]
        } else if (num == "worst") {
            hospital <- sorted_subset[last_row, 1]
        } else if (num < 1 || num > last_row) {
            hospital <- "NA"
        } else {
            hospital <- sorted_subset[num, 1]
        }
        
        result[i,1] <- hospital
        result[i,2] <- state
    }
    
    ## sort the result data frame by state name
    result <- result[order(result[, 2]), ]
    result
}


################################################################################
## Sample Runs
################################################################################

plot_mortality_rate()

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
#best("BB", "heart attack")
#best("NY", "hert attack")

rank_hospital("MD", "heart failure", 5)
rank_hospital("TX", "heart failure", 4)
rank_hospital("MD", "heart attack", "worst")
rank_hospital("MN", "heart attack", 5000)

result <- rank_all("heart attack", 20)
head(result, 10)
result <- rank_all("pneumonia", "worst")
tail(result, 3)
result <- rank_all("heart failure")
tail(result, 10)

################################################################################