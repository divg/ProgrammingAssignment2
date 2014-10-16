## Coursera R programming assignment 2  
## Cache inverse of a matrix 


# makeCacheMatrix creates a special matrix object that can 
# cache its inverse, which is really a list of functions to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
        
}

## cacheSolve computes the inverse of a square invertible matrix 
## by retrieving it from the cache (if its already been computed before
## and the matrix has not changed) OR with the solve function 
## Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}