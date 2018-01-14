################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 1
## File: week-01-lessons.R
################################################################################


## download_files()
## read_local_files()
## read_excel_files()
## read_xml()
## read_json()
## use_data_table()


############################################################
## Downloading Files
############################################################

downlaod_files <- function() {

    ########################################
    # Getting/setting working directory
    ########################################
    
    print("Working directory:")
    print(getwd())
    cat("\n")
    
    ########################################
    # Checking for/creating directories
    ########################################
    
    if (!file.exists("data")) {
        dir.create("data")
    }
    
    ########################################
    # Downloading a file from the web
    ########################################
    
    url <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
    download.file(url, destfile="data/cameras.csv", method="curl")
    date_downloaded <- date()
    
    print("Files in data directory:")
    print(list.files("data"))
    cat("\n")

}


############################################################
## Reading Local Files
############################################################

read_local_files <- function() {

    ########################################
    # Loading flat files - read.table()
    ########################################
    
    camera_data <- read.table("data/cameras.csv", sep=",", header=TRUE)
    
    print("Camera data loaded using read.table():")
    print(head(camera_data))
    cat("\n")
    
    camera_data <- read.csv("data/cameras.csv")
    
    print("Camera data loaded using read.csv():")
    print(head(camera_data))
    cat("\n")
    
    ########################################
    # Further notes
    ########################################
    
    # quote - quoted values (e.g., quote="")
    # na.strings - character representing missing values
    # nrows - how many rows to read
    # skip - number of lines to skip before starting to read

}        


############################################################
## Reading Excel Files
############################################################

read_excel_files <- function() {

    ########################################
    # Downloading file to load
    ########################################
    
    url <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
    download.file(url, destfile="data/cameras.xlsx", method="curl")
    date_downloaded <- date()
    
    ########################################
    # read.xlsx(), read.xlsx2()
    ########################################
    
    install.packages("xlsx")
    library(xlsx)
    
    camera_data <- read.xlsx("data/cameras.xlsx", sheetIndex=1, header=TRUE)
    
    print("Camera data loaded using read.xlsx():")
    print(head(camera_data))
    cat("\n")
    
    ########################################
    # Reading specific rows and cols
    ########################################
    
    col_index <- 2:3
    row_index <- 1:4
    camera_data_subset <- read.xlsx("data/cameras.xlsx", sheetIndex=1,
                                    colIndex=col_index, rowIndex=row_index)
    
    print("Camera data subset:")
    print(camera_data_subset)
    cat("\n")
    
    ########################################
    # Further notes
    ########################################
    
    # write.xlsx function will write out Excel file
    # read.xlsx2() is much faster, but may be unstable for reading subsets of rows
    # XLConnect package has more options for writing and manipulating Excel files
    # XLConnect vignette is a good place to start
    # In general, store data in .csv or .tab/.txt for easy readability

}


############################################################
## Reading XML
############################################################

read_xml <- function() {

    ########################################
    # Reading files into R
    ########################################
    
    #install.packages("XML")
    library(XML)
    
    url <- "http://www.acm.org/about/class/1998/acmccs98-1.2.3.xml"
    doc <- xmlTreeParse(url, useInternal=TRUE)
    rootnode <- xmlRoot(doc)
    
    print("Root node:")
    print(xmlName(rootnode))
    cat("\n")
    
    print("Names:")
    print(names(rootnode))
    cat("\n")
    
    ########################################
    # Accessing parts of XML document
    ########################################
    
    print("rootnode[[1]]:")
    #print(rootnode[[1]])
    cat("\n")
    
    print("rootnode[[1]][1]]:")
    print(rootnode[[1]][[1]])
    cat("\n")
    
    ########################################
    # Extracting parts of file
    ########################################
    
    print("xmlSApply(rootnode, xmlValue):")
    #print(xmlSApply(rootnode, xmlValue))
    cat("\n")
    
    ########################################
    # XPath
    ########################################
    
    # /node -- top level node
    # //node -- node at any level
    # node[@attr-name] -- node with an attribute name
    # node[@attr-name='bob'] -- node with attribute name attr-name='bob'
    # http://www.stat.berkeley.edu/~statcur/Workshop2/Presentations/XML.pdf
    
    ########################################
    # Getting items
    ########################################
    
    print("xpathSApply(rootnode, //name, xmlValue):")
    print(xpathSApply(rootnode, "//name", xmlValue))
    cat("\n")
    
    ########################################
    # Extracting content by attributes
    ########################################
    
    url <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
    doc <- htmlTreeParse(url, useInternal=TRUE)
    scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
    teams <- xpathSApply(doc, "//li[@class='team-name']", xmlValue)
    
    print("Scores:")
    print(scores)
    print("Teams:")
    print(teams)

}


############################################################
## Reading JSON
############################################################

read_json <- function() {

    ########################################
    # Reading data from JSON
    ########################################
    
    #install.packages("jsonlite")
    library(jsonlite)
    
    json_data <- fromJSON("https://api.github.com/users/jtleek/repos")
    
    print("names(json_data):")
    print(names(json_data))
    cat("\n")
    
    ########################################
    # Nested objects in JSON
    ########################################
    
    print("names(json_data$owner):")
    print(names(json_data$owner))
    cat("\n")
    
    print("json_data$owner$login:")
    print(json_data$owner$login)
    cat("\n")
    
    ########################################
    # Writing data frames to JSON
    ########################################
    
    my_json <- toJSON(iris, pretty=TRUE)
    
    print("toJOSN(iris, pretty=TRUE):")
    #cat(my_json)
    cat("\n")
    
    ########################################
    # Converting back to JSON
    ########################################
    
    iris2 <- fromJSON(my_json)
    
    print("head(iris2):")
    print(head(iris2))
    cat("\n")
    
    ########################################
    # Further resources
    ########################################
    
    # http://www.json.org/
    # http://www.r-bloggers.com/new-package-jsonlite-a-smarter-json-encoderdecoder/
    # jsonlite vignette

}


############################################################
## Using data.table
############################################################

use_data_table <- function() {

    ########################################
    # data.table
    ########################################
    
    # data.table inherits from data.frame
    # written in C, so much faster
    # much, much faster at subsetting, grouping, and updating
    
    ########################################
    # Creating data tables
    ########################################
    
    #install.packages("data.table")
    library(data.table)
    
    DF <- data.frame(x=rnorm(9), y=rep(c("a","b","c"), each=3), z=rnorm(9))
    
    print("Data frame:")
    print(head(DF,3))
    cat("\n")
    
    DT <- data.table(x=rnorm(9), y=rep(c("a","b","c"), each=3), z=rnorm(9))
    
    print("Data table:")
    print(head(DT, 3))
    cat("\n")
    
    ########################################
    # Selecting all data tables in memory
    ########################################
    
    print("Tables in memory:")
    print(tables())
    cat("\n")
    
    ########################################
    # Subsetting rows
    ########################################
    
    print("DT[2,]:")
    print(DT[2,])
    cat("\n")
    
    print("DT[DT$y=='a',]")
    print(DT[DT$y=='a',])
    cat("\n")
    
    print("DT[c(2,3)]")
    print(DT[c(2,3)])
    cat("\n")
    
    ########################################
    # Calculating values for variables with expressions
    ########################################
    
    print("DT[, list(mean(x), sum(z))]:")
    print(DT[, list(mean(x), sum(z))])
    cat("\n")
    
    print("DT[, table(y)]:")
    print(DT[, table(y)])
    cat("\n")
    
    ########################################
    # Adding new columns
    ########################################
    
    print("DT[, w:= z^2]:")
    print(DT[, w:= z^2])
    cat("\n")
    
    DT2 <- DT
    print("DT[, y:=2]:")
    print(DT[, y:=2])
    cat("\n")
    
    print("head(DT, n=3):")
    print(head(DT, n=3))
    cat("\n")
    
    print("head(DT2, n=3):")
    print(head(DT2, n=3))
    cat("\n")
    
    ########################################
    # Multiple operations
    ########################################
    
    print("DT[, m:= {tmp <- (x+z); log2(tmp+5)}]:")
    print(DT[, m:= {tmp <- (x+z); log2(tmp+5)}])
    cat("\n")
    
    ########################################
    # plyr-like operations
    ########################################
    
    print("DT[, a:=x>0]:")
    print(DT[, a:=x>0])
    cat("\n")
    
    print("DT[, b:=mean(x+w), by=a]:")
    print(DT[, b:=mean(x+w), by=a])
    cat("\n")
    
    ########################################
    # Special variables
    ########################################
    
    # .N -- an integer, length 1, containing the number r
    
    set.seed(123);
    DT <- data.table(x=sample(letters[1:3], 1E5, TRUE))
    
    print("DT[, .N, by=x]:")
    print(DT[, .N, by=x])
    cat("\n")
    
    ########################################
    # Keys
    ########################################
    
    DT <- data.table(x=rep(c("a","b","c"), each=100), y=rnorm(300))
    setkey(DT, x)
    
    print("DT['a']:")
    print(DT['a'])
    cat("\n")
    
    ########################################
    # Joins
    ########################################
    
    DT1 <- data.table(x=c('a','a','b','dt1'), y=1:4)
    DT2 <- data.table(x=c('a','b','dt2'), z=5:7)
    setkey(DT1, x)
    setkey(DT2, x)
    
    print("merge(DT1, DT2):")
    print(merge(DT1, DT2))
    cat("\n")
    
    ########################################
    # Fast reading
    ########################################
    
    big_df <- data.frame(x=rnorm(1E6), y=rnorm(1E6))
    file <- tempfile()
    write.table(big_df, file=file, row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)
    
    print("system.time(fread(file)):")
    print(system.time(fread(file)))
    cat("\n")
    
    print("system.time(read.table(file)):")
    print(system.time(read.table(file, header=TRUE, sep="\t")))
    cat("\n")
    
    ########################################
    # Further resources
    ########################################
    
    # Latest version contains new functions like melt and dcast for data.table:
    # https://r-forge.r-project.org/scm/viewcv.php/pkg/NEWS?view=markup&root=datatable
    # List of differences between data.table and data.frame:
    # http://stackoverflow.com/questions/13618488/what-you-can-do-with-data-frame-that-you-cant-in-data-table
    # Notes based on Raphael Gottardo's notes:
    # https://github.com/raphg/Biostat-578/master/Advanced_data_manipulation.Rpres

}        

################################################################################