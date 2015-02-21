## R functions to cache potentially time-consuming Matrix inversion computations

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse.  
  #
  # Args:
  #   x:  Should be a square invertible matrix such as the results of:
  #       matrix(c(4,3,3,2), nrow=2, ncol=2).
  #       The default value is 1x1 matrix with value NA).
  #
  # Usage example:  x <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))
  #
  # Returns:
  #   The special "matrix" object represented as a list of 
  #   functions that can be performed on it.
  
  m <- NULL     # Used to store the inverted matrix
  set <- function(y) {
    # Resets x and m.  
    #
    # Args:
    #   y:  Should be a square invertible matrix (default value is 1x1 matrix with value NA)
    x <<- y
    m <<- NULL
  }
  get <- function() {
    # Returns: The matrix x.  
    x
  }
  setInverse <- function(inverse){
    # Sets m to the given inverse of x 
    #
    # Args:
    #   inverse:  Should be a square invertible matrix (and the exact inversion of x)
    m <<- inverse
  } 
  getInverse <- function(){
    # Returns: The inverted matrix of x. 
    m
  } 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


cacheSolve <- function(x, ...) {
  # Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse from the cache.
  #
  # Args:
  #   x:  A makeCacheMatrix object
  #
  # Usage Example: Given x from makeCacheMatrix: y <- cacheSolve(x)
  #
  # Returns:
  #   A matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  # If the inverse has already been calculated, return it.
  if(!is.null(m)) {
    message("Retrieving cached data...")
    return(m)
  }
  
  # Otherwise compute the inverse of a square matrix using the solve function. 
  # This presumes that x is a square invertible matrix such as the results of:
  # matrix(c(4,3,3,2), nrow=2, ncol=2). Expected inverse would be
  # matrix(c(-2,3,3,-4), nrow=2, ncol=2)
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m #return the inverse.
}
