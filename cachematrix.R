## Series of methods designed to calculate the inverse 
## of a given square matrix and store/cache that 
## calculation for later use, without requiring 
## any subsequent calculations (calculate once, use many)

## Provides a container for storing the original matrix
## as well as its inversion - done so be returning a list
## of functions that provide getter/setter capabilities
## to the "container"
## arg x - square matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<-NULL
    }
    get <- function() x
    setinv <- function(invM) inv <<- invM
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Given list result of makeCacheMatrix call as 
## the input, this calculates the inverse of 
## the square matrix within the makeCacheMatrix
## container if and only if it has not been 
## previously calculated.  Returns the inverted matrix
## arg x - result of the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Getting cached inverted matrix")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
