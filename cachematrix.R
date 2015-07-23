## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set matrix x
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        # get (return) matrix x
        get <- function() x
        # set inverse matrix
        setinv <- function(i) inv <<- i
        # get inverse matrix
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
