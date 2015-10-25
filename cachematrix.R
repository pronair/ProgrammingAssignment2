## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  inv <- NULL
  
  # store a matrix
    set <- function(y) {
    x <<- y
    # since the matrix is assigned a new value, flush the cache
    inv <<- NULL
    }
    
  # returns the stored matrix
  get <- function() {
    x
  }
  
  # cache the given argument 
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # get the cached value
  getInverse <- function() {
    inv
  }
  # return a list. Each named element of the list is a function
       list(set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' ( get the cached value)
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise get the matrix, caclulate the inverse and store it in the cache
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}



