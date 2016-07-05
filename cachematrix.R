## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.the list contains four functions :
## set: assign new value of matrix
## get: pass through the present matrix value
## setinv: assign the new inverse of a matrix
## getinv: pass through the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(re) inv <<- re
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## the cachesolve get the cache data first,if not ,it will calculate the 
## inverse of a matrix, and cache in special "matrix"(the list).
cacheSolve <- function(x, ...) {
  y <- x$getinv()
  if(!is.null(y)) {
    message("getting cached data")
    return(y)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}