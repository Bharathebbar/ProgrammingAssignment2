## Caching the Inverse of a Matrix
## Matrix Inverse is a time consuming operation hence it is beneficial caching it rather to compute it each and everytime.
## Followed below are pair of functions that are used to create a special object that stores a matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix" object, that caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getinverse()
       if(!is.null(inv)){
         message("getting cached data")
         return(inv)
       }
       data <- x$get()
       inv <- solve(data,...)
       x$setinverse(inv)
       inv
}
