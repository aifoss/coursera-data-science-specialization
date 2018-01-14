################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 5 Reproducible Research
## Week: Week 2
## File: week-02-lessons.R
## Date: 2016-02-15
################################################################################


################################################################################
## Coding Standards in R
################################################################################

################################################################################
## Markdown
################################################################################

############################################################
## Markdown Syntax
############################################################

## italics: *...* 
## bold: **...**
## main heading: # ...
## secondary heading: ## ...
## tertiary heading: ### ...
## unordered list: - ...
## ordered list: 1. ...
## link: [text] (link)
## advanced linking:
## ... [text][1] ... [text][2]
## [1]: link "text"
## [2]: link "text"
## newline: double space after the end of line

################################################################################
## R Markdown
################################################################################

################################################################################
## knitr
################################################################################

##################################################
## Creating/viewing markdown file from R
##################################################

library(knitr)
setwd(".")
knit2html("document.Rmd")
browseURL("document.html")

##################################################
## Suppressing eval results from being displayed
##################################################

# ```{r results=FALSE}

##################################################
## Inline text computation
##################################################

# ```{r computetime, echod=FALSE}
# time <- format(Sys.time(), "%a %b %d %X %Y")
# rand <- rnorm(1)
# ```
# The current time is `r time`. My favorite random number is `r rand`.

##################################################
## Making a table with xtable
##################################################

# ```{r showtable, results="asis"}
# library(xtable)
# xt <- xtable(summary(fit))
# print(xt, type="html")
# ```

##################################################
## Setting global options
##################################################

# ```{r setoptions, echo=FALSE}
# opts_chunk$set(echo=FALSE, results="hide")
# ```

##################################################
## Some common options
##################################################

# Output
#   - results: "asis", "hide"
#   - echo: TRUE, FALSE
# Figures
#   - fig.height: numeric
#   - fig.width: numeric

##################################################
## Caching computations
##################################################

# cache=TRUE option can be set on a chunk-by-chunk basis to store results
#   of computation

################################################################################