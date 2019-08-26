## Assignment: Caching the Inverse of a Matrix
##
## Following two functions improve performance of
## matrix invertion by caching the results of the
## first matrix solving.

## Create an instance of the "cacheMatrix" class
## wrapping an matrix x.
## "cacheMatrix" is designed to cache the invert
## of the matrix, obtailed by the solve() function.
## To calculate or get cached invert of the matrix x
## use cacheSolve() function
##
## Available operations with the instance of cacheMatrix
## - get()  - get wrapped matrix
## - set(x) - set new matrix x. Previously cached inverse
##            matrix will be cleared.
## - getinverse()  - get cached inverse of the wrapped matrix
##                   or NULL if no inverse matrix was yet calculated
## - setinverse(i) - set cached inverse matrix to be i
##
## @param x matrix to wrap
## @return instance of "cacheMatrix"
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(m) {
            x <<- m
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return the inverse of the cacheMatrix "x".
## Solve the matrix, if not already solved.
##
## x - instance of the "cacheMatrix", created with makeCacheMatrix(...)
## Return inverse of the matrix c (class "matrix")
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("Using cached matrix")
            i
        } else {
            m <- x$get()
            i <- solve(m, ...)
            x$setinverse(i)
            i
        }
}
