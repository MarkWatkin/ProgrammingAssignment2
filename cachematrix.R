## ====================================================================
## R Programming
## Programming Assignment 2: Lexical Scoping
## ====================================================================
## This file contains logic that allows the inverse of a matrix to be calculated and then cached for later retrieval
## makeCacheMatrix returns a new object that acts as a wrapper to an existing matrix
## cashSolve should be called passing the wrapper to the matrix in question and returns the inverse to that matrix
## ====================================================================
## makeCacheMatrix
## This function returns an object that is like a wrapper to the passed matrix
## This wrapper is compatable with the cacheSolve method which behaves like the solve() function except it is more performant through caching results
## ====================================================================
makeCacheMatrix <- function(x = matrix()) {
  ## Initialise the inverse as null
  inverse <-NULL
  set_matrix <- function(in_matrix) {
    x <<-in_matrix
    inverse <<-NULL
  }
  get_matrix <- function() {
    x
  }
  set_inverse <- function(in_inverse) {
    inverse <<-in_inverse
  }
  get_inverse <- function() {
    inverse
  }
  list (set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}
## ====================================================================
## cacheSolve
## This function behaves like the solve() function, which returns the inverse of a matrix
## It may be more performant than solve() as it caches the result and reuse the cached value on subsequent calls
## Rather than passing a standard matrix this function works on a special wrapper of a matrix created by calling makeCacheMatrix
## ====================================================================
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get_matrix()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse  
}
