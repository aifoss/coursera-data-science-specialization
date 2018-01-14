################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 3
## File: week-03-lessons.R
################################################################################


## subset_and_sort()
## summarize_data()
## create_new_variables()
## reshape_data()
## dplyr_data()
## merge_data()


#################################################################
## Subsetting and Sorting
#################################################################

subset_and_sort <- function() {
    
    ##################################################
    # Subsetting - quick review
    ##################################################

    set.seed(12345)
    X <- data.frame("var1"=sample(1:5), "var2"=sample(6:10), "var3"=sample(11:15))
    X <- X[sample(1:5), ]
    X$var2[c(1,3)] <- NA
    
    print("X:")
    print(X)
    cat("\n")
    
    firstCol <- X[, 1]
    
    print("First column:")
    print(firstCol)
    cat("\n")
    
    var1Col <- X[, "var1"]
    
    print("var1 column:")
    print(var1Col)
    cat("\n")
    
    var2Col <- X[1:2, "var2"]
    
    print("First 2 rows of var2 column:")
    print(var2Col)
    cat("\n")
    
    ##################################################
    # Logical ANDs and ORs
    ##################################################
    
    subset1 <- X[(X$var1 <= 3 & X$var3 > 11), ]
    
    print("Subset 1:")
    print(subset1)
    cat("\n")
    
    subset2 <- X[(X$var1 <= 3 | X$var3 > 15), ]
    
    print("Subset 2:")
    print(subset2)
    cat("\n")
    
    ##################################################
    # Dealing with missing values
    ##################################################
    
    subset3 <- X[which(X$var2 > 8), ]
    
    print("Subset 3:")
    print(subset3)
    cat("\n")
    
    ##################################################
    # Sorting
    ##################################################
    
    sorted1 <- sort(X$var1)
    
    print("Sorted by var1:")
    print(sorted1)
    cat("\n")
    
    sorted2 <- sort(X$var1, decreasing=TRUE)
    
    print("Sorted by var1 in decreasing order:")
    print(sorted2)
    cat("\n")
    
    sorted3 <- sort(X$var2, na.last=TRUE)
    
    print("Sorted by var2 putting NA last:")
    print(sorted3)
    cat("\n")
    
    ##################################################
    # Ordering
    ##################################################
    
    ordered1 <- X[order(X$var1), ]
    
    print("Ordered by var1:")
    print(ordered1)
    cat("\n")
    
    ordered2 <- X[order(X$var1, X$var3), ]
    
    print("Ordered by var1 and var3:")
    print(ordered2)
    cat("\n")
    
    ##################################################
    # Ordering with plyr
    ##################################################
    
    library(plyr)
    
    arranged1 <- arrange(X, var1)
    
    print("Arranged by var1:")
    print(arranged1)
    cat("\n")
    
    arranged2 <- arrange(X, desc(var1))
    
    print("Arranged by var1 in decreasing order:")
    print(arranged2)
    cat("\n")
    
    ##################################################
    # Adding rows and columns
    ##################################################
    
    X$var4 <- rnorm(5)
    
    print("var4 added:")
    print(X)
    cat("\n")
    
    Y <- cbind(X, rnorm(5))
    
    print("cbind-ed:")
    print(Y)
    cat("\n")
    
}


#################################################################
## Summarizing Data
#################################################################

summarize_data <- function() {
    
    ##################################################
    # Getting data from the web
    ##################################################
    
    if (!file.exists("data")) {
        dir.create("data")
    }
    
    url <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
    #download.file(url, destfile="data/restaurants.csv", method="curl")
    restaurant_data <- read.csv("data/restaurants.csv")
    
    ##################################################
    # Looking at a bit of data
    ##################################################
    
    head <- head(restaurant_data, n=3)
    tail <- tail(restaurant_data, n=3)
    
    print("Head of restaurant data:")
    print(head)
    cat("\n")
    
    print("Tail of restaurant data:")
    print(tail)
    cat("\n")
    
    ##################################################
    # Making a summary
    ##################################################
    
    summary <- summary(restaurant_data)
    
    print("Summary of restaurant data:")
    print(summary)
    cat("\n")
    
    ##################################################
    # More in-depth information
    ##################################################
    
    print("Detail of restaurant data:")
    print(str(restaurant_data))
    cat("\n")
    
    ##################################################
    # Quantiles of quantitative variables
    ##################################################
    
    quant1 <- quantile(restaurant_data$councilDistrict, na.rm=TRUE)
    
    print("Quantile 1:")
    print(quant1)
    cat("\n")
    
    quant2 <- quantile(restaurant_data$councilDistrict, 
                       probs=c(0.5,0.75,0.9), na.rm=TRUE)
    
    print("Quantile 2:")
    print(quant2)
    cat("\n")
    
    ##################################################
    # Making table
    ##################################################
    
    table1 <- table(restaurant_data$zipCode, useNA="ifany")
    
    print("Restaurant data table 1:")
    print(table1)
    cat("\n")
    
    table2 <- table(restaurant_data$councilDistrict, restaurant_data$zipCode)
    
    print("Restaurant data table 2:")
    print(table2)
    cat("\n")
    
    ##################################################
    # Checking for missing values
    ##################################################
    
    sum <- sum(is.na(restaurant_data$councilDistrict))
    
    print("Sum of NA council district values in restaurant data:")
    print(sum)
    cat("\n")
    
    any <- any(is.na(restaurant_data$councilDistrict))
    
    print("If any council district values are NA:")
    print(any)
    cat("\n")
    
    all <- all(restaurant_data$zipCode > 0)
    
    print("Whether all zip code values > 0:")
    print(all)
    cat("\n")
    
    ##################################################
    # Row and column sums
    ##################################################
    
    col_sums <- colSums(is.na(restaurant_data))
    
    print("Column sums for NA values:")
    print(col_sums)
    cat("\n")
    
    zero_col_sums <- all(colSums(is.na(restaurant_data)) == 0)
    
    print("Whether all columns for NA values are zero:")
    print(zero_col_sums)
    cat("\n")
    
    ##################################################
    # Values with specific characteristics
    ##################################################
    
    zipcode_data_1 <- table(restaurant_data$zipCode %in% c("21212"))
    
    print("Number of zipcode == 21212:")
    print(zipcode_data_1)
    cat("\n")
    
    zipcode_data_2 <- table(restaurant_data$zipCode %in% c("21212","21213"))
    
    print("Number of zipcode == 21212 | 21213:")
    print(zipcode_data_2)
    cat("\n")
    
    zipcode_subset <- restaurant_data[restaurant_data$zipCode %in% c("21212","21213"), ]
    
    print("Subset with zipcode == 21212 | 21213:")
    print(zipcode_subset)
    cat("\n")
    
    ##################################################
    # Cross tabs
    ##################################################
    
    data(UCBAdmissions)
    DF <- as.data.frame(UCBAdmissions)
    summary_ucb <- summary(DF)
    
    print("Summary of UCB admission data:")
    print(summary_ucb)
    cat("\n")
    
    xt <- xtabs(Freq ~ Gender + Admit, data=DF)
    
    print("UCB data cross tab by gender and admission:")
    print(xt)
    cat("\n")
    
    ##################################################
    # Flat tables
    ##################################################
    
    warpbreaks$replicate <- rep(1:9, len=54)
    xt <- xtabs(breaks ~., data=warpbreaks)
    
    print("Warp breaks data cross tab:")
    print(xt)
    cat("\n")
    
    ftable <- ftable(xt)
    
    print("Flat table:")
    print(ftable)
    cat("\n")
    
    ##################################################
    # Size of dataset
    ##################################################
    
    fake_data <- rnorm(1e5)
    size <- object.size(fake_data)
    
    print("Fake data size:")
    print(size)
    cat("\n")
    
    print("Fake data size in Mb:")
    print(size, units="Mb")
    cat("\n")
    
}


#################################################################
## Creating New Variables
#################################################################

create_new_variables <- function() {
    
    ##################################################
    # Getting data from the web
    ##################################################

    url <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
    #download.file(url, destfile="data/restaurants.csv", method="curl")
    rest_data <- read.csv("data/restaurants.csv")
    
    ##################################################
    # Creating sequences
    ##################################################
    
    s1 <- seq(1,10, by=2)
    
    print("Sequence 1:")
    print(s1)
    cat("\n")
    
    s2 <- seq(1,10, length=3)
    
    print("Sequence 2:")
    print(s2)
    cat("\n")
    
    x <- c(1,3,8,25,100)
    s3 <- seq(along=x)
    
    print("Sequence 3:")
    print(s3)
    cat("\n")
    
    ##################################################
    # Subsetting variables
    ##################################################
    
    rest_data$nearMe <- rest_data$neighborhood %in% c("Roland Park","Homeland")
    table <- table(rest_data$nearMe)
    
    print("Table summarizing number of restaurants near me:")
    print(table)
    cat("\n")
    
    ##################################################
    # Creating binary variables
    ##################################################
    
    rest_data$zipWrong <- ifelse(rest_data$zipCode < 0, TRUE, FALSE)
    table <- table(rest_data$zipWrong, rest_data$zipCode < 0)
    
    print("Table summarizing number of wrong zip codes:")
    print(table)
    cat("\n")
    
    ##################################################
    # Creating categorical variables
    ##################################################
    
    rest_data$zipGroups <- cut(
        rest_data$zipCode, breaks=quantile(rest_data$zipCode))
    table <- table(rest_data$zipGroups)
    
    print("Summary of restaurant data broken down by zip codes:")
    print(table)
    cat("\n")
    
    table <- table(rest_data$zipGroups, rest_data$zipCode)
    
    print("Restaurant data by zip groups and zip code:")
    print(table)
    cat("\n")
    
    ##################################################
    # Easier cutting
    ##################################################
    
    #install.packages("Hmisc")
    library(Hmisc)
    
    rest_data$zipGroups <- cut2(rest_data$zipCode, g=4)
    table <- table(rest_data$zipGroups)
    
    print("Restaurant data broken down by 4 zip groups:")
    print(table)
    cat("\n")
    
    ##################################################
    # Creating factor variables
    ##################################################
    
    rest_data$zcf <- factor(rest_data$zipCode)
    factors <- rest_data$zcf[1:10]
    
    print("First 10 zip code factors in restaurant data:")
    print(factors)
    cat("\n")
    
    ##################################################
    # Levels of factor variables
    ##################################################
    
    yesno <- sample(c("yes","no"), size=10, replace=TRUE)
    yesnofac <- factor(yesno, levels=c("yes","no"))
    yesnofac <- relevel(yesnofac, ref="yes")
    yesnofacnum <- as.numeric(yesnofac)
    
    print("yes/no factors:")
    print(yesnofac)
    cat("\n")
    
    print("yes/no factors as numeric:")
    print(yesnofacnum)
    cat("\n")
    
    ##################################################
    # Cutting produces factor variables
    ##################################################
    
    library(Hmisc)
    
    rest_data$zipGroups <- cut2(rest_data$zipCode, g=4)
    table <- table(rest_data$zipGroups)
    
    print("Zip groups by cut2():")
    print(table)
    cat("\n")
    
    ##################################################
    # Using mutate function
    ##################################################
    
    library(Hmisc)
    library(plyr)
    
    rest_data_2 <- mutate(rest_data, zipGroups=cut2(zipCode, g=4))
    table <- table(rest_data_2$zipGroups)
    
    print("Zip groups by mutate():")
    print(table)
    cat("\n")
    
    ##################################################
    # Common transforms
    ##################################################
    
    # abs(x) -- absolute value
    # sqrt(x) -- square root
    # ceiling(x) -- ceiling(3.475) is 4
    # floor(x) -- floor(3.475) is 3
    # round(x, digits=n) -- round(3.475, digits=2) is 3.48
    # signif(x, digits=n) -- signif(3.475, digits=2) is 3.5
    # cos(x), sin(x), etc.
    # log(x) -- natural logarithm
    # log2(x), log10(x)
    # exp(x) -- exponentiating x
    
    ##################################################
    # Further resources
    ##################################################
    
    # http://www.biostat.jhsph.edu/~ajaffe/lec_winterR/Lecture%202.pdf
    # http://statmethods.net/management/functions.html
    # http://plyr.had.co.nz/09-user/
    
}


#################################################################
## Reshaping Data
#################################################################

reshape_data <- function() {
    
    ##################################################
    # Starting with reshaping
    ##################################################
 
    library(reshape2)
    
    head <- head(mtcars)
    
    print("Head of mtcars data:")
    print(head)
    cat("\n")
    
    ##################################################
    # Melting data frames
    ##################################################
    
    mtcars$carname <- rownames(mtcars)
    car_melt <- melt(mtcars, 
                    id=c("carname","gear","cyl"), 
                    measure.vars=c("mpg","hp"))
    head <- head(car_melt, n=3)
    tail <- tail(car_melt, n=3)
    
    print("Head of car_melt data:")
    print(head)
    cat("\n")
    
    print("Tail of car_melt data:")
    print(tail)
    cat("\n")
    
    ##################################################
    # Casting data frames
    ##################################################
    
    cyl_data <- dcast(car_melt, cyl ~ variable)
    
    print("Cyl data:")
    print(cyl_data)
    cat("\n")
    
    cyl_mean_data <- dcast(car_melt, cyl ~ variable, mean)
    print("Cyl mean data:")
    print(cyl_mean_data)
    cat("\n")
    
    ##################################################
    # Averaging values
    ##################################################
    
    head <- head(InsectSprays)
    
    print("Head of insect spray data:")
    print(head)
    cat("\n")
    
    count <- tapply(InsectSprays$count, InsectSprays$spray, sum)
    
    print("Counts of insect sprays:")
    print(count)
    cat("\n")
    
    ##################################################
    # Another way - split
    ##################################################
    
    sp_ins <- split(InsectSprays$count, InsectSprays$spray)
    
    print("Split of insect sprays:")
    print(sp_ins)
    cat("\n")
    
    spr_count <- lapply(sp_ins, sum)
    
    print("Sum of insect sprays by split:")
    print(spr_count)
    cat("\n")
    
    ##################################################
    # Another way - combine
    ##################################################
    
    unlist <- unlist(spr_count)
    
    print("Sum of sprays using unlist():")
    print(unlist)
    cat("\n")
    
    sp_ins_sum <- sapply(sp_ins, sum)
    
    print("Sum of sprays using sapply():")
    print(sp_ins_sum)
    cat("\n")
    
    ##################################################
    # Another way - plyr package
    ##################################################
    
    dd <- ddply(InsectSprays, .(spray), summarize, 
                sum=sum(count, FUN=sum), by=count)
    
    print("Sum of sprays using ddply():")
    print(dd)
    cat("\n")
    
    ##################################################
    # Creating a new variable
    ##################################################
    
    spray_sums <- ddply(InsectSprays, .(spray), summarize, 
                        sum=ave(count, FUN=sum), by=count)
    dim <- dim(spray_sums)
    head <- head(spray_sums)
    
    print("Dimensions of spray sums:")
    print(dim)
    cat("\n")
    
    print("Head of spray sums:")
    print(head)
    cat("\n")
    
    ##################################################
    # Further resources
    ##################################################
    
    # plyr developer tutorial:
    # http://plyr.had.co.nz/09-user/
    # reshape tutorial:
    # http://www.slideshare.net/jeffreybreen/reshaping-data-in-r
    # plyr primer:
    # http://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/
    # See also functions:
    # acast -- for casting as multi-dimensional arrays
    # arrange -- for faster reordering without using order() commands
    # mutate -- for adding new variables
    
}    


#################################################################
## Managing Data Frames with dplyr
#################################################################

dplyr_data <- function() {
    
    ##################################################
    # dplyr
    ##################################################
    
    # Optimized and distilled version of plyr package
    # Provides a "grammar" for data manipulation
    # Very fast as many key operations are coded in C++
    
    ##################################################
    # dplyr verbs
    ##################################################
    
    # select -- return a subset of the columns of a data frame
    # filter -- extract a subset of rows from a data frame based on logical conditions
    # arrange -- reorder rows of a data frame
    # rename -- rename variables in a data frame
    # mutate -- add new variables/columns or transform existing variables
    # summarize -- generate summary statistics of different variables in a data frame
    
    ##################################################
    # dplyr properties
    ##################################################
    
    # First argument is a data frame
    # Subsequent arguments describe what to do with it
    # You can refer to columns in the data frame directy, without using $
    # Result is a new data frame
    
    ##################################################
    # Loading dplyr package
    ##################################################
    
    #install.packages("dplyr")
    library(dplyr)
    
    ##################################################
    # Select
    ##################################################
    
    #install.packages("RDS")
    library(RDS)
    
    rest_data <- read.csv("data/restaurants.csv")
    rest_rds <- saveRDS(rest_data, "data/rest.rds")
    rest_rds_data <- readRDS("data/rest.rds")
    dim <- dim(rest_rds_data)
    head <- head(select(rest_rds_data, 1:5))
    
    print("Dimensions of restaurant data:")
    print(dim)
    cat("\n")
    
    print("Head of restaurant data:")
    print(head)
    cat("\n")
    
    names <- names(rest_rds_data)[1:3]
    
    print("First 3 names in restaurant data:")
    print(names)
    cat("\n")
    
    cols <- head(select(rest_rds_data, name:neighborhood))
    
    print("Head of first 3 columns:")
    print(cols)
    cat("\n")
    
    other_cols <- head(select(rest_rds_data, -(name:neighborhood)))
    
    # i <- match("name", names(rest_rds_data))
    # j <- amtch("neighborhood", names(rest_rds_data))
    # head(rest_rds_data[, -(i:j)])
    
    print("Head of the other columns:")
    print(other_cols)
    cat("\n")
    
    ##################################################
    # Filter
    ##################################################
    
    subset <- filter(rest_rds_data, zipCode > 21210)
    head <- head(select(subset, 1:3, policeDistrict), 5)
    
    print("Subset with zipCode > 21210")
    print(head)
    cat("\n")
    
    subset <- filter(rest_rds_data, 
                     zipCode > 21230 & policeDistrict == 'SOUTHEASTERN')
    head <- head(select(subset, 1:4, policeDistrict), 10)
    
    print("Subset with zipCode > 21210 & policeDistrict == 'SOUTHEASTERN':")
    print(head)
    cat("\n")
    
    ##################################################
    # Arrange
    ##################################################
    
    rest_data_rds <- arrange(rest_rds_data, zipCode)
    head <- head(select(rest_data_rds, name, zipCode), 5)
    tail <- tail(select(rest_data_rds, name, zipCode), 5)
    
    print("Head of restaurant data rearranged by zip code:")
    print(head)
    cat("\n")
    
    print("Tail of restaurant data rearranged by zip code:")
    print(tail)
    cat("\n")
    
    rest_data_rds <- arrange(rest_rds_data, desc(zipCode))
    head <- head(select(rest_data_rds, name, zipCode), 5)
    tail <- tail(select(rest_data_rds, name, zipCode), 5)
    
    print("Head of restaurant data rearranged by descending zip codes:")
    print(head)
    cat("\n")
    
    print("Tail of restaurant data rearranged by descending zip codes:")
    print(tail)
    cat("\n")
    
    ##################################################
    # Rename
    ##################################################
    
    head <- head(rest_rds_data[, 1:5], 3)
    
    print("Head before renaming variables:")
    print(head)
    cat("\n")
    
    rest_rds_data <- rename(rest_rds_data,
                          cDistrict = councilDistrict,
                          pDistrict = policeDistrict)
    head <- head(rest_rds_data[, 1:5], 3)
    
    print("Head after renaming variables:")
    print(head)
    cat("\n")
    
    ##################################################
    # Mutate
    ##################################################
    
    rest_rds_data <- mutate(rest_rds_data, 
                            zipcodeTrend=zipCode-mean(zipCode, na.rm=TRUE))
    head <- head(select(rest_rds_data, zipCode, zipcodeTrend))
    
    print("Head after mutating:")
    print(head)
    cat("\n")
    
    ##################################################
    # Group_by
    ##################################################
    
    rest_rds_data <- mutate(rest_rds_data,
                          region = factor(1 * (zipCode > 21230),
                                          labels=c("lower", "upper")))
    lower_upper <- group_by(rest_rds_data, region)
    summary <- summarize(lower_upper,
                         mean=mean(zipCode, na.rm=TRUE),
                         max=max(zipCode),
                         median=median(zipCode))
    
    print("Summary after grouping by zipcodes:")
    print(summary)
    cat("\n")
    
    ##################################################
    # %>%
    ##################################################
    
    rest_rds_data %>% 
        mutate(region = factor(1 * (zipCode > 21220), labels=c("L", "U"))) %>%
        group_by(region) %>%
        summarize(mean=mean(zipCode, na.rm=TRUE),
                    max=max(zipCode),
                    median=median(zipCode))
    
    print("Summary after grouping by zipcodes using %>%:")
    print(summary)
    cat("\n")
    
}


#################################################################
## Merging Data
#################################################################

merge_data <- function() {
    
    ##################################################
    # Peer review data
    ##################################################
    
    url1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
    url2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
    #download.file(url1, destfile="data/reviews.csv", method="curl")
    #download.file(url2, destfile="data/solutions.csv", method="curl")
    
    reviews <- read.csv("data/reviews.csv")
    solutions <- read.csv("data/solutions.csv")
    rHead <- head(reviews, 2)
    sHead <- head(solutions, 2)
    
    print("Head of review data:")
    print(rHead)
    cat("\n")
    
    print("Head of solution data:")
    print(sHead)
    cat("\n")
    
    ##################################################
    # Merging data - merge()
    ##################################################
    
    # Merges data frames
    # Important parameters: x, y, by, by.x, by.y, all
    
    r_names <- names(reviews)
    s_names <- names(solutions)
    
    print("Names in review data:")
    print(r_names)
    cat("\n")
    
    print("Names in solution data:")
    print(s_names)
    cat("\n")
    
    merged_data <- merge(reviews, solutions, 
                         by.x="solution_id", by.y="id", all=TRUE)
    m_head <- head(merged_data)
    
    print("Head of merged data:")
    print(m_head)
    cat("\n")
    
    ##################################################
    # Default - merging all common column names
    ##################################################
    
    intersection <- intersect(s_names, r_names)
    
    print("Intersection of names in review data and solution data:")
    print(intersection)
    cat("\n")
    
    merged_data_2 <- merge(reviews, solutions, all=TRUE)
    m_head_2 <- head(merged_data_2)
    
    print("Head of merged data 2:")
    print(m_head_2)
    cat("\n")
    
    ##################################################
    # Using join in the plyr package
    ##################################################
    
    # Defaults to left join
    
    df1 <- data.frame(id=sample(1:10), x=rnorm(10))
    df2 <- data.frame(id=sample(1:10), y=rnorm(10))
    joined <- arrange(join(df1, df2), id)
    
    print("Two data frames joined by id:")
    print(joined)
    cat("\n")
    
    ##################################################
    # Joining multiple data frames
    ##################################################
    
    df3 <- data.frame(id=sample(1:10), z=rnorm(10))
    df_list <- list(df1, df2, df3)
    all_joined <- join_all(df_list)
    
    print("Three data frames joined by id:")
    print(all_joined)
    cat("\n")
    
    ##################################################
    # Further resources
    ##################################################
    
    # R data merging page:
    # http://www.statmethods.net/management/merging.html
    # plyr information:
    # http://plyr.had.co.nz/
    
}

################################################################################