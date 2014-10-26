## The following functions, makeCacheMatrix and cacheSolve, cache the result
## of the inverse of a matrix in order to save potentially time-consuming
## recomputation. 

## makeCacheMatrix caches a matrix object and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function() { x }
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    getInverse <- function() { inv }
    
    setInverse <- function(inverse) { inv <<- inverse }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix from makeCacheMatrix.
## It returns the cache inverse stored in makeCacheMatirx if
## makeCasheMatrix$getInverse() returns a non-NULL value, else it
## computes the inverse using the solve() as well as caches the result
## in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m 
}
