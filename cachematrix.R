###########
## Programming Assignment 2: Lexical Scoping
##
## The following code has been written for the second programming assignment 
## in the "R Programming" course on coursera conducted by Roger D. Peng, 
## Johns Hopkins University.
##
## Description:
## The functions makeCacheMatrix and cacheSolve help to cache the inverse of 
## a matrix, a computation which is computationally costly if done repeatedly. 
###########

###########
## Function 1: makeCacheMatrix
## This function creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

###########
## Function 2: cacheSolve
## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated and the 
## matrix has not changed, then cacheSolve retrieves the inverse from 
## the cache. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    print(data)
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
