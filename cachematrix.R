## These fucntions, makeCacheMatrix and cacheSolve, cache the inverse of a 
## matrix. These functions use lexical scoping.

## makeCacheMatrix is afunction that creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        # setting the value of the vector
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        # getting the value of the vector
        get <- function() x
        # setting the value of the inverse
        setinverse <- function(inverse) invrs <<- inverse
        # getting the value of the inverse
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function to return an inverse matrix that is returned by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        matrix <- x$get()
        invrs <- solve(matrix, ...)
        x$setinverse(invrs)
        invrs
}
