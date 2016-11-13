## These functions demonstrate use of R scoping rules to cache 

## makeCacheMatrix(x) wraps an invertible matrix 'x' in an object that
## stores the original matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(xi) i <<- xi
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve(x) returns the inverse of a matrix that is
## cached in the object 'x' created by makeCacheMatrix.  
## If the inverse hasn't been already cached, it will calculate it
## using solve() and cache the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
