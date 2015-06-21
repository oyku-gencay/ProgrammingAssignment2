## This is a cache implementation of R function solve.
## We first prepare a data structure to cache solved result
## Then provide a caching solve function  

## makeCacheMatrix prepares a data structure to get and set the matrix and cached result
makeCacheMatrix <- function(x = matrix()) {
    # empty the cache
    cache <- NULL
    set <- function(y){
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setInverse <- function(inv){
        cache <<- inv
    }
    getInverse <- function() cache
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve is a caching solve function. If we already have an inverse, returns it.
cacheSolve <- function(x, ...) {    
    # Try to get the cached inverse if exists
    inv <- x$getInverse()
    if(!is.null(inv)){
        # We have a cached copy of the inverse, return it
        message("getting cached inverse")
        return inv
    } else {
        # We do not have a cached inverse.         
        # calculate, cache and return
        matrix <- x$get()        
        inv <- solve(matrix)
        x$setInverse(inv)
        inv
    }
}
