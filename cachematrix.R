## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## These functions cache the inverse of a matrix so that the
## (potentially costly) inverse is computed only once and reused.

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                       # holds the cached inverse

        set <- function(y) {              # reset the matrix and clear the cache
                x <<- y
                inv <<- NULL
        }
        get <- function() x               # return the matrix

        setinverse <- function(inverse) inv <<- inverse   # store the inverse
        getinverse <- function() inv                      # return cached inverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: returns the inverse of the special "matrix" from makeCacheMatrix.
## If the inverse is already cached and the matrix is unchanged, it returns the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)           # compute the inverse
        x$setinverse(inv)                 # cache it
        inv
}
