################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 4
## File: week-04-lessons.R
################################################################################


## edit_text_variables()
## work_with_dates()


#################################################################
## Editing Text Variables
#################################################################

edit_text_variables <- function() {
    
    ##################################################
    # Editing text variables
    ##################################################

    url <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
    file <- "data/cameras.csv"
    #download.file(url, file, method="curl")
    
    camera_data <- read.csv(file)
    names <- names(camera_data)
    lc_names <- tolower(names)
    
    print("Names in camera data:")
    print(names)
    cat("\n")
    
    print("Names in lower case:")
    print(lc_names)
    cat("\n")
    
    ##################################################
    # Fixing character vectors - strsplit()
    ##################################################
    
    # Good for automatically splitting variable names
    # Important parameters: x, split
    
    split_names <- strsplit(names, "\\.")
    split_name_5 <- split_names[[5]]
    split_name_6 <- split_names[[6]]
    
    print("5th name split:")
    print(split_name_5)
    cat("\n")
    
    print("6th name split:")
    print(split_name_6)
    cat("\n")
    
    ##################################################
    # Quick aside - lists
    ##################################################
    
    mylist <- list(letters=c("A","b","c"), numbers=1:3, matrix(1:25, ncol=5))
    mylist_head <- head(mylist)
    mylist_1 <- mylist[1]
    mylist_letters <- mylist$letters
    mylist_eleme_1 <- mylist[[1]]
    
    print("Head of mylist:")
    print(mylist_head)
    cat("\n")
    
    print("mylist[1]:")
    print(mylist_1)
    cat("\n")
    
    print("mylist$letters:")
    print(mylist_letters)
    cat("\n")
    
    print("mylist[[1]]:")
    print(mylist_eleme_1)
    cat("\n")
    
    ##################################################
    # Fixing character vectors - sapply()
    ##################################################
    
    # Applies a function to each element in a vector or list
    # Important parameters: X, FUN
    
    split_name_6_1 <- split_names[[6]][1]
    
    print("split_names[[6]][1]:")
    print(split_name_6_1)
    cat("\n")
    
    first_element <- function(x){x[1]}
    first_element_split <- sapply(split_names, first_element)
    
    print("First elements of split_names:")
    print(first_element_split)
    cat("\n")
    
    ##################################################
    # Peer review data
    ##################################################
    
    url1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
    url2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
    file1 <- "data/reviews.csv"
    file2 <- "data/solutions.csv"
    method <- "curl"
    #download.file(url1, file1, method)
    #download.file(url2, file2, method)
    
    reviews <- read.csv(file1)
    solutions <- read.csv(file2)
    reviews_head <- head(reviews, 2)
    solutions_head <- head(solutions, 2)
    
    print("Head of review data:")
    print(reviews_head)
    cat("\n")
    
    print("Head of solution data:")
    print(solutions_head)
    cat("\n")
    
    ##################################################
    # Fixing character variables - sub()
    ##################################################
    
    # Important parameters: pattern, replacement, x
    
    review_names <- names(reviews)
    
    print("Review data names:")
    print(review_names)
    cat("\n")
    
    subst <- sub("_", "", review_names)
    
    print("review data names without underscores:")
    print(subst)
    cat("\n")
    
    ##################################################
    # Fixing character variables - gsub()
    ##################################################
    
    test_name <- "this_is_a_test"
    sub <- sub("_", "", test_name)
    gsub <- gsub("_", "", test_name)
    
    print("Test name with sub() applied:")
    print(sub)
    cat("\n")
    
    print("Test name with gsub() applied:")
    print(gsub)
    cat("\n")
    
    ##################################################
    # Fixing values - grep(), grepl()
    ##################################################
    
    alameda <- grep("Alameda", camera_data$intersection)
    
    print("Alameda found in camera data:")
    print(alameda)
    cat("\n")
    
    alameda_tbl <- table(grepl("Alameda", camera_data$intersection))
    
    print("Alameda grepl table:")
    print(alameda_tbl)
    cat("\n")
    
    camera_data_2 <- camera_data[!grepl("Alameda", camera_data$intersection), ]
    head <- head(camera_data_2)
    
    print("Head of camera data without Alameda:")
    print(head)
    cat("\n")
    
    ##################################################
    # More on grep
    ##################################################
    
    alameda_true <- grep("Alameda", camera_data$intersection, value=TRUE)
    
    print("Camera data with Alameda:")
    print(alameda_true)
    cat("\n")
    
    jeff_st <- grep("Jeff Street", camera_data$intersection)
    
    print("Camera data with Jeff Street:")
    print(jeff_st)
    cat("\n")
    
    jeff_st_len <- length(jeff_st)
    
    print("JeffStreet data length:")
    print(jeff_st_len)
    cat("\n")
    
    ##################################################
    # More useful string functions
    ##################################################
    
    #install.packages("stringr")
    library(stringr)
    
    fname <- "Jeffrey"
    lname <- "Leek"
    name <- paste(fname, lname)
    num_chars <- nchar(name)
    substr <- substr(name, 1, 7)
    
    print("Name:")
    print(name)
    cat("\n")
    
    print("Number of characters in name:")
    print(num_chars)
    cat("\n")
    
    print("Substring with [1,7]:")
    print(substr)
    cat("\n")
    
    name0 <- paste0(fname, lname)
    
    print("Name pasted with paste0():")
    print(name0)
    cat("\n")
    
    trim <- str_trim("Jeff          ")
    
    print("'Jeff     ' trimmed:")
    print(trim)
    cat("\n")
    
    ##################################################
    # Important points about text in data sets
    ##################################################
    
    # Names of variables should be:
    #   all lower case when possible
    #   Descriptive
    #   Not duplicated
    #   Not containing underscores or dots or white spaces
    # Variables with character values should:
    #   usually be made into factor variables
    #   be descriptive
    
}


#################################################################
## Regular Expressions
#################################################################
    
    ##################################################
    # Literals
    ##################################################
    
    # Simplest pattern consists only of literals.

    ##################################################
    # Metacharacters
    ##################################################

    # ^ -- start of a line
    # e.g. ^i think
    
    # $ -- end of a line
    # e.g. morning$
    
    ##################################################
    # Character classes with []
    ##################################################

    # We can list a set of characters we will accept at a given point in the match
    # e.g. [Bb][Uu][Ss][Hh]
    # e.g. ^[Ii] am
    # e.g. ^[0-9][a-zA-Z]
    
    # When used at the beginning of a character class,
    #   ^ indicates matching characters NOT in the indicated class
    # e.g. [^?.]$

    ##################################################
    # More metacharacters
    ##################################################

    # "." is used to refer to any character
    # e.g. 9.11
    
    # "|" is used to represent "or"
    # e.g. flood|fire
    
    # The alternatives can be expressions and not just literals
    # e.g. ^[Gg]ood|[Bb]ad
    
    # Subexpressions are often contained in parentheses to constrain the alternatives
    # e.g. ^([Gg]ood|[Bb]ad)
    
    # "?" indicates that the indicated expression is optional
    # e.g. [Gg]eorge( [Ww]\.)? [Bb]ush
    
    # "*" means any number, including none
    # e.g. (.*)
    
    # "+" means at least one of the items
    # e.g. [0-9]+ (.*)[0-9]+
    
    # "{" and "}" are referred to as interval quantifiers
    #   they let us specify the minimum and maximum number of matches of an expression
    # e.g. [Bb]ush( +[^ ]+ +){1,5} debate
    
    # m,n means at least m but not more than n matches
    # m means exactly m matches
    # m, means at least m matches
    
    # "(" and ") can also be used to remember text matched by the subexpression enclosed
    #   We refer to the matched text with \1, \2, etc.
    # e.g. +)[a-zA-Z]+) +\1 +
    
    # "*" is greedy so it always matches the longest possible string
    #   that satisfies the regular expression
    # e.g. ^s(.*)s
    # The greediness can be turned off with "?"
    # e.g. ^s(.*?)s$


#################################################################
## Working with Dates
#################################################################

work_with_dates <- function() {

    ##################################################
    # Starting simple
    ##################################################

    d1 <- date()
    d1_class <- class(d1)
    
    print("Current date using date():")
    print(d1)
    cat("\n")
    
    print("Class of date object:")
    print(d1_class)
    cat("\n")
    
    ##################################################
    # Date class
    ##################################################
    
    d2 <- Sys.Date()
    d2_class <- class(d2)
    
    print("Current date using Sys.Date():")
    print(d2)
    cat("\n")
    
    print("Class of date object:")
    print(d2_class)
    cat("\n")
    
    ##################################################
    # Formatting dates
    ##################################################
    
    # %d -- day as number (0-31)
    # %a -- abbreviated weekday
    # %A -- unabbreviated weekday
    # %m -- month (00-12)
    # %b -- abbreviated month
    # %B -- unabbreviated month
    # %y -- 2-digit year
    # %Y -- 4-digit year
    
    d2_form <- format(d2, "%a %b %d")
    
    print("Formated date:")
    print(d2_form)
    cat("\n")
    
    ##################################################
    # Creating dates
    ##################################################
    
    x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
    z <- as.Date(x, "%d%b%Y")
    
    print("Dates:")
    print(z)
    cat("\n")
    
    diff <- z[1] - z[2]
    num_diff <- as.numeric(diff)
    
    print("Difference between first and second dates:")
    print(diff)
    cat("\n")
    
    print("Numeric difference between first and second dates:")
    print(num_diff)
    cat("\n")
    
    ##################################################
    # Converting to Julian
    ##################################################
    
    d2_day <- weekdays(d2)
    d2_month <- months(d2)
    d2_julian <- julian(d2)
    
    print("Weekday of current date:")
    print(d2_day)
    cat("\n")
    
    print("Month of current date:")
    print(d2_month)
    cat("\n")
    
    print("Julian of current date:")
    print(d2_julian)
    cat("\n")
    
    ##################################################
    # lubridate
    ##################################################
 
    library(lubridate)
    
    ymd <- ymd("20140108")
    mdy <- mdy("08/04/2013")
    dmy <- dmy("03-04-2013")
    
    print("20140108:")
    print(ymd)
    cat("\n")
    
    print("08/04/2013:")
    print(mdy)
    cat("\n")
    
    print("03-04-2013:")
    print(dmy)
    cat("\n")
    
    ##################################################
    # Dealing with times
    ##################################################
    
    ymd_hms <- ymd_hms("2011-08-03 10:15:03")
    ymd_hms_2 <- ymd_hms("2011-08-03 10:15:03", tz="Pacific/Auckland")
    timezone <- Sys.timezone()
    
    print("2011-08-13 10:15:03:")
    print(ymd_hms)
    cat("\n")
    
    print("2011-08-13 10:15:03 tz='Pacific/Auckland':")
    print(ymd_hms_2)
    cat("\n")
    
    print("Timezone:")
    print(timezone)
    cat("\n")
    
    ##################################################
    # Some functions having slightly different syntax
    ##################################################
    
    x <- dmy(c("1jan2013", "2jan2013", "31mar2013", "30jul2013"))
    wday_num <- wday(x[1])
    wday_char <- wday(x[1], label=TRUE)
    
    print("Weekday as numeric:")
    print(wday_num)
    cat("\n")
    
    print("Weekday as character:")
    print(wday_char)
    cat("\n")
    
    ##################################################
    # Further resources
    ##################################################
    
    # lubridate tutorial:
    # http://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1.1.0/
    # lubridate vignette:
    # http://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
       
}


#################################################################
## Data Resources
#################################################################

    ##################################################
    # Open government sites
    ##################################################

    # United Nations: http://data.un.org/
    # US: http://www.data.gov/
    # UK: http://data.gov.uk/
    # France: http://www.data.gouv.fr.
    # Ghana: http://data.gov.gh/
    # Australia: http://data.gov.au/
    # Germany: https://www.govdata.de/
    # Hong Kong: http://www.gov.hk/en/theme/psi/datasets/
    # Japan: http://www.data.go.jp/
    # Many more: http://www.data.gov/opendatasites

    ##################################################
    # Gapminder
    ##################################################
    
    # http://www.gapminder.org/
    
    ##################################################
    # Survey data from the US
    ##################################################
    
    # http://www.asdfree.com/
    
    ##################################################
    # Infochimps marketplace
    ##################################################
    
    # http://www.infochimps.com/marketplace
    
    ##################################################
    # Kaggle
    ##################################################
    
    # http://www.kaggle.com/
    
    ##################################################
    # Collections by data scientists
    ##################################################
    
    # Hilary Mason:
    # http://bitly.com/bundles/hmason/1
    # Peter Skomoroch:
    # https://delicious.com/pskomoroch/dataset
    # Jeff Hammberbacher:
    # http://www.quora.com/Jeff-Hammerbacher/Introduction-to-Data-Science-Data-Sets
    # Gregory Piatetsky-Shapiro:
    # http://www.kdnuggets.com/gps.html
    # http://blog.mortardata.com/post/67652898761/6-dataset-lists-curated-by-data-scientists
    
    ##################################################
    # More specialized collections
    ##################################################
    
    # Stanford Large Network Data
    # UCI Machine Learning
    # KDD Nuggets Datasets
    # CMU Statlib
    # Gene expression omnibus
    # ArXiv Data
    # Public data sets on Amazon Web Services
    
    ##################################################
    # Some APIs with R interfaces
    ##################################################
    
    # twitter and twitteR
    # figshare and rfigshare
    # PLoS and rplos
    # rOpenSci
    # Facebook and RFacebook
    # Google maps and RGoogleMaps

################################################################################