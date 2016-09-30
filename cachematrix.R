## second draft

## Caching the Inverse of a Matrix:
## Matrix inverssion is usually a costly computation and there may be some 
## benefit to caching the inverseerse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseerse) inverse <<- inverseerse
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve function computes the inverseerse of the special "matrix" created by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then it should retrieve the inverseerse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverseerse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setInverse(inverse)
        inverse
}
