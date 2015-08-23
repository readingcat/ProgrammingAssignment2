## PREAMBLE:
## =============================================================================
##
## This script contains 2 functions:
##
##      makeCacheMatrix()   - This function creates a special "matrix" object
##                            that can cache its inverse;
##
##      cacheSolve()        - This function computes the inverse of the special
##                            "matrix" returned by makeCacheMatrix() above. If
##                            the inverse has already been calculated (and the
##                            matrix has not changed), then this function 
##                            retrieves the inverse from the cache.
##
## Make sure that the matrix supplied is invertible!
##
## =============================================================================





## makeCacheMatrix(): This function creates a special "matrix" object that
## can cache its inverse.
## =============================================================================

makeCacheMatrix <- function(x = matrix()) {
  
  # begin by setting the value of invx to NULL
  invx <- NULL
  
  # set the value of matrix x to a new matrix y, and reset the value of invx to NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  
  # get the value of matrix x
  get <- function() x
  
  # set the value of invx to invmat
  setinv <- function(invmat) {
    invx <<- invmat
  }
  
  # get the value of invx
  getinv <- function() invx
  
  # return the special "matrix"
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}





## cacheSolve(): This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix() above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve() retrieves
## the inverse from the cache.
## =============================================================================

cacheSolve <- function(x, ...) {
  
  # return a matrix that is the inverse of x
  invx <- x$getinv()
  
  # if the inverse has already been calculated, retrieve invx from the cache
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  
  # else, i.e., if the invx is NULL, compute the inverse
  data <- x$get()
  invx <- solve(data, ...)
  
  # cache the computed inverse
  x$setinv(invx)
  
  # return the computed inverse
  invx
}
