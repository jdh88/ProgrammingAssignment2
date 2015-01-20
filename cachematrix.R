## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#creates a special "matrix" object that can cache its inverse
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


## Write a short comment describing this function
#computes teh inverse of the special "matrix" returned by makeCacheMatrix
#if the inverse has already been calculated, the previous result is returned
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
