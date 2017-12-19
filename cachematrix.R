## Functions for creating a matrix object that can cache its inverse
## and calculating the inverse

## makeCacheMatrix:
## Creates an object that can store a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(a) inv <<- a
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve:
## Calculates the inverse of a matrix created by the makeCacheMatrix function.
## If the inverse is already calculated the cached inverse is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
