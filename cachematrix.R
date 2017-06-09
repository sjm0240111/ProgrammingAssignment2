## This function typically calculate the inverse of a matrix. And in order to 
## make the program faster, we cache the result of the inverse since calculating
## the inverse is time consuming.

## The first function cache the matrix and it's inverse, And it provides the sub
## function to reset the matrix and its inverse. Note that this function can set
## only one matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(i) {
        inv <<- i
    }
    getinv <- function() {
        inv
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This simple function first checks if the inverse of the matrix exists, if it 
## exists, then just return it. If the inverse dosen't exist, then calculate the
## inverse and set it in the cache as well as return it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
