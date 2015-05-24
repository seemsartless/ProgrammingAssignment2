## David Sky - seemsArtless homework for Programming Assignment #2 - May 22, 2015
## 
## Will use the "superassignment" operator, <<- to cache the values up in the global environment

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Based on the assignment description, modified for a matrix
    # These are the helper functions that deal with the global variables
    # and related functions.
    
    # Commented out the message statements, they were just for debugging and test purposes
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    # The function that actually computes the inverse of the
    # matrix and sets it to m
    setmatInv <- function(solve) {
        # message( "debug: In setmatInv...")
        m <<- solve
    }
    
    getmatInv <- function() {
        # message( "debug: In getmatInv...")
        m }
    
    # Return the four functions we'll need
    list(set = set, 
         get = get,
         setmatInv = setmatInv,
         getmatInv = getmatInv
    )
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then this function will retrieve the inverse from the cache rather
## than calculating it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Uses functions and global variables defined above in makeCacheMatrix
    
    # Start by assuming we DO have a cache'd value from a previous use of this inverse
    m <- x$getmatInv()

    if(!is.null(m)) { # If something was found for m in the cache, just return that
        message("getting cached data")
        return(m)
    }
    # We only get here if m IS null, ie not found in the cache, so we
    # need to do the calculation, AND set the result in the cache so we can
    # find it directly next time without re-calculating it.
    
    # message("debug: is.null returned true, so do the calculation")
    data <- x$get()
    m <- solve(data, ...)
    x$setmatInv(m)
    m
    
}
