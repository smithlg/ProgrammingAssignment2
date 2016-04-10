# This script provides functions to calculate inverse of matrix and 
# and more importantly to provide memoization without
# use of a separate package. See timing information below.

# makeCacheMatrix() is a closure function with required formal of x, a matrix
# and returning the following functions
# set(): to set the matrix, get(): to get the matrix; matrix stored as variable y
# setinv(): to set the inverse of the matrix, inverse stored as variable cache
# getinv(): to retrieve the inverse of the matrix, if set 
# makeCacheMatrix() must be initialized as an object with the matrix
# before cacheSolve() to store the functions and matrix in the function's environment

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setinv <- function(inv) cache <<- inv
    getinv <- function() cache
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
## Runtime test example
## diag() creates square matrix, using non 0 numbers
## in main diagonal which assures successful inverse calculation
# d <- diag(c(2, 1, 8))
# m <- makeCacheMatrix(d)
# > m
# $set
# function (y) 
# {
#    x <<- y
#    cache <<- NULL
# }
# <environment: 0x000000000b733d60>
#
# $get
# function () 
# x
# <environment: 0x0000000015bc3840>
#    
# $setinv
# function (inv) 
# cache <<- inv
# <environment: 0x000000000b733d60>
#        
# $getinv
# function () 
# cache
# <environment: 0x000000000b733d60>
#
# > m$get()
# [,1] [,2] [,3]
# [1,]    2    0    0
# [2,]    0    1    0
# [3,]    0    0    8

## cacheSolve() has a required formal of x the global object created by makeCacheMatrix()
## cacheSolve determines if variable inv, which is the inverse of the matrix/array,
## is in the objects environment and either retrieves inverse, if available, or calculates inverse 
## and sets it in the environment

cacheSolve <- function(x, ...) {
    ## If test to determine if calculated inverse has been cached
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## If a calculated result is not cached to calculate and cache result
    data <- x$get()
    inv <- solve(data, ...) # function solve() evaluates to inverse of matrix
    x$setinv(inv)
    inv
}
## Run Time Test Sample continued
## First Pass of cacheSolve()
# > cacheSolve(m)
# [,1] [,2]  [,3]
# [1,]  0.5    0 0.000
# [2,]  0.0    1 0.000
#[3,]  0.0    0 0.125
## Second Pass of cacheSolve()
# > cacheSolve(m)
# getting cached data
# [,1] [,2]  [,3]
# [1,]  0.5    0 0.000
# [2,]  0.0    1 0.000
# [3,]  0.0    0 0.125

## Timing Test
## system.time() shows function run time
## test shows improved speed of execution from cache
## using non trivial array of 1000x1000
# > d <- diag(1:1000)
# > m <- makeCacheMatrix(d)
# > system.time(cacheSolve(m)) ## first function calculation
#   user  system elapsed 
#   0.24    0.00    0.23 
# > system.time(cacheSolve(m)) ## second function from cache
# getting cached data
#   user  system elapsed 
#      0       0       0 



