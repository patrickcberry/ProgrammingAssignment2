## Two functions to calculate the inverse of a matrix. The inverse matrix
## is cached 

## #######################################################################
## makeCacheMatrix
##
## Creates a cached matrix 'object' to store the matrix data and the 
## inverse matrix. 
##
## Returns: a list of functions

makeCacheMatrix <- function(x = matrix()) {

    # variable to store the inverse matrix
    inv <- NULL
    
    # set new matrix data for the object
    set <- function(y) {
        x <<- y         # replace the matrix data
        inv <<- NULL    # clear the cached inverse of the matrix 
    }
    
    # get the matrix data
    get <- function() x
    
    # set the inverse matrix
    setinv <- function( inverse ) inv <<- inverse
    
    # get the inverse matrix
    getinv <- function() inv
    
    # list the available functions
    list( set = set, get = get, setinv = setinv, getinv = getinv )
}

## #######################################################################
## cacheSolve
##
## Takes a list created with the makeCacheMatrix function and returns the 
## inverse matrix. If the invese has been calculated previously then a cached
## value will be returned
##
## Returns: a matrix

cacheSolve <- function(x, ...) {
    
    # get the cached value
    inv <- x$getinv()
    
    if( !is.null(inv) ) {
        message("returning cached inverse matrix")
        return( inv )   # return the cached value
    }
    
    # cached value is NULL. Need to calculate the inverse and cache the value
    
    message("returning calculated inverse matrix")
    m <- x$get()        # get the matrix
    inv <- solve(m)     # calculate the inverse
    x$setinv(inv)       # set the cached value
    inv                 # return the calculated inverse
}
