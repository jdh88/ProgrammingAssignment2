## This file contains two functions that manipulate a "matrix object"
#The object allows for caching of a matrix's inverse as this can
#be a computationally expensive task.  

## makeCacheMatrix(matrix)
#creates a special "matrix" object that can cache its inverse
#contains four functions that can act on it to get and set values
#of the inverted and uninverted matrix.  This constructor can be
#used as follows:
#
# testmatrix <- matrix(c(1,2,5,7), nrow = 2, ncol = 2)
# cmat <- makeCacheMatrix()
# cmat$set(testmatrix)
# or
# cmat <- makeCacheMatrix(testmatrix)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) inv <<- solve
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## cachSolve(cacheMatrixObject)
#computes the inverse of the special "matrix" returned by makeCacheMatrix
#if the inverse has already been calculated, the previous result is returned.
#It can be used as follows:
#
# cachSolve(cmat)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$get_inverse()
      if(!is.null(inv)){
          message("Getting cached data")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$set_inverse(inv)
      inv
}
