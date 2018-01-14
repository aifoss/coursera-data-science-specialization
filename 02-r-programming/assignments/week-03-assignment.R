################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 2 R Programming
## Week: Week 3
## Assignment: Assignment 2
## File: week-03-assignment.R
## Date: 2016-01-09
################################################################################


################################################################################
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that we will 
## not discuss here). Your assignment is to write a pair of functions 
## that cache the inverse of a matrix.
################################################################################

################################################################################
## function make_cache_matrix()
## This function creates a special "matrix" object that can cache its inverse.
################################################################################

make_cache_matrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    set_inverse <- function(invrs) {
        inverse <<- invrs
    }
    get_inverse <- function() {
        inverse
    }
    list(
        set=set,
        get=get,
        set_inverse=set_inverse,
        get_inverse=get_inverse)
}

################################################################################
## function cache_solve()
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
################################################################################

cache_solve <- function(x, ...) {
    inverse <- x$get_inverse()
    if (!is.null(inverse)) {
        message("getting cached matrix inverse")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$set_inverse(inverse)
    inverse
}


################################################################################
## In this example we introduce the <<- operator which can be used 
## to assign a value to an object in an environment that is different 
## from the current environment. Below are two functions that are used 
## to create a special object that stores a numeric vector 
## and cache's its mean.
################################################################################

################################################################################
## The first function, make_vector creates a special "vector", 
## which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
################################################################################

make_vector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

################################################################################
## The following function calculates the mean of the special "vector" 
## created with the above function. However, it first checks to see 
## if the mean has already been calculated. If so, it gets the mean 
## from the cache and skips the computation. Otherwise, it calculates 
## the mean of the data and sets the value of the mean in the cache 
## via the setmean function.
################################################################################

cache_mean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


################################################################################
## Sample Runs
################################################################################

x <- matrix(c(1,2,0,3,2,1,0,2,3), nrow=3, ncol=3, byrow=TRUE)
mat <- make_cache_matrix(x)
inv <- cache_solve(mat)
inv

################################################################################