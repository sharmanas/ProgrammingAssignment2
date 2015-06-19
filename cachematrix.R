## These functions make the computation of matrix inverse more effective by
## caching the inverse of a matrix rather than computing it repeatedly

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns a matrix 'm' that is the inverse of 'x'
## Note: This function assumes that the matrix is always convertible

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
