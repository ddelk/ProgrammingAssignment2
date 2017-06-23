## This function will take an input matrix and give its inverse as output.
## The inverse matrix will come from cache if it has been inversed once
## otherwise it will inverse the input matrix.

## This function will take an input matrix and create a similar dimensioned
## output matrix. 



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(solve) m <<- solve
    getmean <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function will determine if the inverse of the input matrix already
## exists in cache. If it does then the cache inverse matrix is returned
## otherwise an inverse of the input matrix is created and cached in memory.


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
