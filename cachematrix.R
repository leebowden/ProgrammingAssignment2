## These functions compute and cache the inverse of a matrix. 
## Matrix inversion is a computationally costly process. 
## The ability to cache a result and look it up, rather than computing 
##it each time can save significant time



## This function creates a special matrix which is really a list 
## containing a function to

##      - set the value of the matrix
##      - get the value of the matrix
##      - set the value of the inverse
##      - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the "matrix" returned by the 
## first function. If the inverse has already been calculated and the 
## matrix has not changed the function will retrieve the inverse from 
## the cache directly. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
