## Generates the "matrix" list containing four functions:
##    set(y) - sets the matrix to specified invertible matrix y
##    get - returns the matrix y
##    setinv(inv) - caches the matrix inverse
##    getinv - returns the cached matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Either computes the matrix inverse or retrieves cached matrix inverse.
## The inverse will be stored in x.
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
