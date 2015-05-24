## David Sky - seemsArtless homework for Programming Assignment #2 - May 22, 2015
## 
## Will use the "superassignment" operator, <<- to cache the values up in the global environment

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Based on the assignment description, modified for a matrix
    # These are the helper functions that deal with the global variables
    # and related functions.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then this function will retrieve the inverse from the cache rather
## than calculating it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Uses functions and global variables defined above in makeCacheMatrix
    
    # Assume we DO have a cache'd value
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
    
}
