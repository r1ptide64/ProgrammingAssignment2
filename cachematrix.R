## These functions allow the caller to create a special matrix that can
## cache its inverse, so that it does not need to be recalculated every
## time it is needed.

## makeCacheMatrix must be called to create the special matrix.
## It expects a single input parameter -- an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve will return the inverse of a matrix created by makeCacheMatrix
## If the inverse has already been calculated, it will return the cahced
## value along with a descriptive message.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("This is the cached inverse.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}