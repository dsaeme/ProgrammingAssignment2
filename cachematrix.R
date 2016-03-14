## Assignment: Caching the ierse of a Matrix
## This file contains a pair of functions that
## cache the ierse of a matrix.

## This function creates a special "matrix" object
## that can cache its ierse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}

## This function computes the ierse of the special
## "matrix" returned by `makeCacheMatrix` above. If the ierse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the ierse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}