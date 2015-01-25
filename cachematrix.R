## The first function creates a special "matrix" object than can cache its inverse
## It is important that is to caching the inverse rather than compute it repeatedly
## This function it can be changed and be used with other functions (mean, sqrt, etc)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x   <<- y
                inv <<- NULL
        }
        get        <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special 'matrix' returned by the above function
## Important: Return a matrix that is the inverse of 'x' if 'x' is a square invertible matrix
## Cache value (looked up when someone need it) is better than recomputed. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data of matrix")
                return(inv)
        }
        data <- x$get()
        inv  <- solve(data, ...)
        x$setinverse(inv)
        inv
}
