## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## setMatrix      set the value of a matrix
## getMatrix      get the value of a matrix
## cacheInverse   cache the cahced inverse of the matrix
## getInverse     get the cahced inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
  # holds the cached value 
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(new) {
    x <<- new
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() x
  
  # cache the given argument 
  cacheInverse <- function(tocache) cache <<- tocache
  
  # get the cached value
  getInverse <- function() cache
  
  # return a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(y, ...) {
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in the cache
  dat <- y$getMatrix()
  inverse <- solve(dat)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}
