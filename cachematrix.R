## These functions work together to store a matrix and then cache its inverse. 

## makeCacheMatrix is passed a matrix argument and returns a list of functions 
## It contains the following functions:
## $set(y)
## sets the stored matrix to the new value passed through y
## $get()
## returns the stored matrix
## $getinv()
## returns the inverse of the matrix, if it has been set
## $setinv(inverse)
## initializes/sets the stored inverse to the passed value

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     getinv <- function() inv
     setinv <- function(inverse) inv <<- inverse
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve works in tandemo with makeCacheMatrix. Its argument x should be a makeCacheMatrix object.
## It solves the inverse of the matrix stored in a makeCacheMatrix object. If the inverse is already solved and stored, it will use the cached inverse and state it is doing so, saving processing time.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}


