## The following functions will cache the inverse of a matrix

## This function create a special "matrix" object that can cache its inverse. 
## A list is returned containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the Inverse of a Matrix
## 4. get the Inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
              x <<- y
              inv <<- NULL
       }
       get <- function() x
       setinv <- function(inversa) {
                     inv <<- inversa
       }
       getinv <- function() inv
       list(set = set, get = get,
            setinv = setinv, 
            getinv = getinv)
}

## This function computes the inverse of the matrix returned by the makeCacheMatrix function. 
## The inverse is retrieved from the cache if it has already been calculated 
## and the matrix has no changed

cacheSolve <- function(x, ...) {
       inv <- x$getinv() 
       if (!is.null(inv)) {
              message("getting cached data")
              return(inv)
       }
       data <- x$get()
       inv <- solve(data)
       x$setinv(inv)
       inv
}
