## ==================================================================
## ProgrammingAssignment #2
## ==================================================================
## Author: d25higgins
## Incept: Dec 6, 2014
## Description:
## ==================================================================
## Two functions for inverting and caching a matrix in
## order to speed repetative computations.
## ==================================================================
## Edit History:
##
## ==================================================================

## function to create a list of functions to manipulate a matrix (invert)

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    get <- function(){x}
    setInverse <- function(solve){im <<- solve}
    getInverse <- function(){im}
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## function to return cached inverted matrix or to create one and save it.

cacheSolve <- function(x, ...) {
    im <- x$getInverse()
    if(!is.null(im)){
        message("getting inverse matrix")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInverse(im)
    im
}
