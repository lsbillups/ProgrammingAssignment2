## This pair of functions compute the inverse of an invertible
## matrix and cache the inverse for the future use. If the inverse 
## of the same matrix is needed, no new computation is necessary.

## makeCacheMatrix creates a list of four functions if
## an invertible matrix is given, and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve works on the object makeCacheMatrix
## generated, computes the inverse if the inverse has
## not been computed before. If the inverse has been 
## calculated, then cacheSolve returns a message and
## the cached inverse.


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv ## Return a matrix that is the inverse of 'x'
}