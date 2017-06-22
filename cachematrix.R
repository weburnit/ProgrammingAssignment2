## Put comments here that give an overall description of what your
## functions do
 a matrix in which hold inv as inversed vector/list
## Create

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Whenever cacheSolve is hooked, it will cached inversed version immediately


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(is.null(m)) {
                message("writing cached data")
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(m)
        }
        m
}
