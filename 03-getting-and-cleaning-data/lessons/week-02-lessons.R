################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 2
## File: week-02-lessons.R
################################################################################


## read_from_mysql()
## read_from_hdf5()
## read_from_web()
## read_from_api()
## read_from_other_sources()


#################################################################
## Reading from MySQL
#################################################################

read_from_mysql <- function() {

    ##################################################
    # Installing RMySQL
    ##################################################
    
    #install.packages("RMySQL")
    #install.packages("DBI")
    
    # http://biostat.mc.vanderbilt.edu/wiki/Main/RMySQL
    # http://www.ahschulz.de/2013/07/23/installing-mysql-under-windows/
    
    ##################################################
    # Connecting and listing databases
    ##################################################
    
    library(RMySQL)
    library(DBI)
    
    ucsc_db <- dbConnect(MySQL(), user="genome",
                         host="genome-mysql.cse.ucsc.edu")
    result <- dbGetQuery(ucsc_db, "show databases;")
    dbDisconnect(ucsc_db)
    
    print("Databases in Genome:")
    print(result)
    cat("\n")
    
    ##################################################
    # Connecting to hg19 and listing tables
    ##################################################
    
    hg19 <- dbConnect(MySQL(), user="genome", db="hg19",
                      host="genome-mysql.cse.ucsc.edu")
    all_tables <- dbListTables(hg19)
    
    print("Number of tables in hg19 DB:")
    print(length(all_tables))
    cat("\n")
    
    print("Names of first 5 tables in hg19 DB:")
    print(all_tables[1:5])
    cat("\n")
    
    ##################################################
    # Getting dimensions of a specific table
    ##################################################
    
    print("affyU133Plus2 table fields:")
    print(dbListFields(hg19, "affyU133Plus2"))
    cat("\n")
    
    print("Number of records in affyU133Plus2 table:")
    print(dbGetQuery(hg19, "select count(*) from affyU133Plus2"))
    cat("\n")
    
    ##################################################
    # Reading from table
    ##################################################
    
    affy_data <- dbReadTable(hg19, "affyU133Plus2")
    
    print("Head of affy_data:")
    print(head(affy_data))
    cat("\n")
    
    ##################################################
    # Selecting a specific subset
    ##################################################
    
    query <- dbSendQuery(hg19,
                         "select * from affyU133Plus2 where misMatches between 1 and 3")
    affy_mis <- fetch(query)
    
    print("Quantile of mismatch in affy_data:")
    print(quantile(affy_mis$misMatches))
    cat("\n")
    
    affy_mis_small <- fetch(query, n=10)
    dbClearResult(query)
    
    print("Dimensions of affy_mis_small:")
    print(dim(affy_mis_small))
    cat("\n")
    
    dbDisconnect(hg19)
    
    ##################################################
    # Further resources
    ##################################################
    
    # RMySQL vigentte:
    # http://cran.r-project.org/web/packages/RMySQL/RMySQL.pdf
    # List of commands:
    # http://www.pantz.org/software/mysql/mysqlcommands.html
    # Summary of other commands:
    # http://www.r-bloggers.com/mysql-and-r/

}


#################################################################
## Reading from HDF5
#################################################################

read_from_hdf5 <- function() {

    ##################################################
    # HDF5
    ##################################################
    
    # Used for storing large data sets
    # Supports storing a range of data types
    # Hierarchical data format
    # groups -- contain 0+ data sets and metadata
    #   Have a group header with group name and list of attributes
    #   Have a group symbol table with a list of objects in group
    # datasets -- multidimensional array of data elements with metadata
    #   Have a header with name, datatype, dataspace, and storage layout
    #   Have a data array with data
    
    ##################################################
    # R HDF5 package
    ##################################################
    
    #source("http://bioconductor.org/biocLite.R")
    #biocLite("rhdf5")
    
    library(rhdf5)
    created <- h5createFile("data/example.h5")
    
    ##################################################
    # Creating groups
    ##################################################
    
    created <- h5createGroup("data/example.h5", "foo")
    created <- h5createGroup("data/example.h5", "baa")
    created <- h5createGroup("data/example.h5", "foo/foobaa")
    
    print("List of groups in example.h5:")
    print(h5ls("data/example.h5"))
    cat("\n")
    
    ##################################################
    # Writing to groups
    ##################################################
    
    A <- matrix(1:10, nr=5, nc=2)
    h5write(A, "data/example.h5", "foo/A")
    B <- array(seq(0.1,2.0, by=0.1), dim=c(5,2,2))
    attr(B, "scale") <- "liter"
    h5write(B, "data/example.h5", "foo/foobaa/B")
    
    print("List of groups in example.h5:")
    print(h5ls("data/example.h5"))
    cat("\n")
    
    ##################################################
    # Writing a dataset
    ##################################################
    
    df <- data.frame(1L:5L, seq(0,1, length.out=5),
                     c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)
    h5write(df, "data/example.h5", "df")
    
    print("List of groups in example.h5:")
    print(h5ls("data/example.h5"))
    cat("\n")
    
    ##################################################
    # Reading data
    ##################################################
    
    readA <- h5read("data/example.h5", "foo/A")
    readB <- h5read("data/example.h5", "foo/foobaa/B")
    readdf <- h5read("data/example.h5", "df")
    
    print("Data in A:")
    print(readA)
    cat("\n")
    
    ##################################################
    # Writing and reading chunks
    ##################################################
    
    h5write(c(12,13,14), "data/example.h5", "foo/A", index=list(1:3,1))
    
    print("Data in A:")
    print(h5read("data/example.h5", "foo/A"))
    cat("\n")
    
    ##################################################
    # Further resources
    ##################################################
    
    # hdf5 can be used to optimize reading/writine from disc in R
    # rhdf5 tutorial:
    # http://www.bioconductor.org/packages/releases/bioc/vignettes/rhdf5/inst/doc/rhdf5.pdf
    # HDF group has information on HDF5 in general:
    # http://www.hdfgroup.org/HDF5/

}


#################################################################
## Reading Data from the Web
#################################################################

read_from_web <- function() {

    ##################################################
    # Getting data off webpages - readLines()
    ##################################################
    
    conn <- url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
    html <- readLines(conn)
    close(conn)
    
    print("Google scholar webpage:")
    #print(html)
    cat("\n")
    
    ##################################################
    # Parsing with XML
    ##################################################
    
    library(XML)
    
    url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
    html <- htmlTreeParse(url, useInternalNodes=T)
    title <- xpathSApply(html, "//title", xmlValue)
    cited_by <- xpathSApply(html, "//td[@id='col-citedby']", xmlValue)
    
    print("Title:")
    print(title)
    cat("\n")
    
    print("Cited by:")
    print(cited_by)
    cat("\n")
    
    ##################################################
    # GET from httr package
    ##################################################
    
    library(httr)
    
    html2 <- GET(url)
    content2 <- content(html2, as="text")
    parsed_html <- htmlParse(content2, asText=TRUE)
    title <- xpathSApply(parsed_html, "//title", xmlValue)
    
    print("Title:")
    print(title)
    cat("\n")
    
    ##################################################
    # Accessing websites with passwords
    ##################################################
    
    pg2 <- GET("http://httpbin.org/basic-auth/user/passwd",
               authenticate("user", "passwd"))
    names <- names(pg2)
    
    print("Response from GET")
    print(pg2)
    cat("\n")
    
    print("Names:")
    print(names)
    cat("\n")
    
    ##################################################
    # Using handles
    ##################################################
    
    google <- handle("http://google.com")
    pg1 <- GET(handle=google, path="/")
    pg2 <- GET(handle=google, path="search")
    
    print("Google page:")
    print(pg1)
    cat("\n")
    
    print("Google search page:")
    print(pg2)
    cat("\n")
    
    ##################################################
    # Further resources
    ##################################################
    
    # Examples of web scraping:
    # http://www.r-bloggers.com/?s=Web-Scraping
    # httr help file:
    # http://cran.r-project.org/web/packages/httr/httr.pdf

}


#################################################################
## Reading from APIs
#################################################################

read_from_api <- function() {
    
    ##################################################
    # Accessing Twitter from R
    ##################################################

    myapp <- oauth_app("twitter",
                       key="yourConsumerKeyHere",
                       secret="yourConsumerSecretHere")
    sig <- sign_oauth1.0(myapp,
                         token="yourTokenHere",
                         token_secret="yourTokenSecretHere")
    home_tl <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)
    
    # https://dev/twitter.com/docs/api/1.1/overview
    # https://dev.twitter.com/docs/api/1.1/get/search/tweets 
    
    ##################################################
    # Converting JSON object
    ##################################################
    
    json1 <- content(home_tl)
    json2 <- jsonlite::fromJSON(toJSON(json1))
    json2[1, 1:4]
    
}    


#################################################################
## Reading from Other Sources
#################################################################

read_from_other_sources <- function() {
    
    ##################################################
    # Interacting more directly with files
    ##################################################
    
    # file -- open a connection to a text file
    # url -- open a connection to a URL
    # gzfile -- open a connection to a .gz file
    # bzfile -- open a connection to a .bz file
    # ?connections for more info
    
    ##################################################
    # foreign package
    ##################################################
    
    # Basic functions read.foo
    #   read.arff -- Weka
    #   read.dta -- Stata
    #   read.mtp -- Minitab
    #   read.ocatve -- Octave
    #   read.spss -- SPSS
    #   read.xport -- SAS
    
    # http://cran.r-project.org/web/packages/foreign/foreign.pdf
    
    ##################################################
    # Examples of other database packages
    ##################################################
    
    # RPostgreSQL provides DBI-compliant database connection from R:
    # https://code.google.com/p/rpostgresql/
    # http://cran.r-project.org/web/packages/RPostgreSQL/RPostgreSQL.pdf
    # RODBC provides interfaces to multiple databases including PostgreSQL:
    # http://cran.r-project.org/web/packages/RODBC/vignettes/RODBC.pdf
    # http://cran.r-project.org/web/packages/RODBC/RODBC.pdf
    # RMongo:
    # http://cran.r-project.org/web/packages/RMongo/RMongo.pdf
    # http://www.r-bloggers.com/r-and-mongodb/
    
    ##################################################
    # Reading images
    ##################################################
    
    # jpeg -- http://cran.r-project.org/web/packages/jpeg/index.html
    # bitmap -- http://cran.r-project.org/web/packages/readbitmap/index.html
    # png -- http://cran.r-project.org/web/packages/png/index.html
    # EBImage -- http://www.bioconductor.org/packages/2.13/bioc/html/EBImage.html
    
    ##################################################
    # Reading GIS data
    ##################################################
    
    # rgdal -- http://cran.r-project.org/web/packages/rgdal/index.html
    # rgeos -- http://cran.r-project.org/web/packages/rgeos/index.html
    # raster -- http:/cran.r-project.org/web/packages/raster/index.html
    
    ##################################################
    # Reading music data
    ##################################################
    
    # tuneR -- http://cran.r-project.org/web/packages/tuneR/
    # seewave -- http://rug.mnhn.fr/seewave/
    
}

################################################################################