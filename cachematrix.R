## Caching the Inverse of a Matrix
## Below are a pair of functions that are used to create an object that 
## stores a matrix and caches its inverse.


## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(a) {
            x <<- a
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## This function computes the inverse of the matrix created by the
## makeCacheMatrix function above. When the inverse has already been  
## calculated then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
    if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
        dat <- x$get()
        inv <- solve(dat, ...)
        x$setinverse(inv)
        inv
}
