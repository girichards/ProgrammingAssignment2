## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## Provides functions
##
## get : returns the current matrix
## set : sets a new matrix, and resets the cached inverse
## getinverse : returns the solve function used to calculate the inverse
## setinverse : sets a new solve function to calculate the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function
## Computes the inverse of a matrix that hasbeen created via 'makeCacheMatrix'
## Returns the cached result of the solve if it has already been calculate, 
## otherwise it calculates, caches and then returns the result of the solve
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if (!is.null(s)) {
        message("getting cached solve")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
