## Computing the inverse of a matrix can be a costly operation.  The two functions
## below return the cached inverse if it exists or computes the inverse and caches
## it if the inverse is not found in the cache.

## The makeCacheMatrix creates a special "matrix" which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function checks to see if there is a cached inverse in the
## cacheMatrix passed in.  If there is a cached inverse, that cached item is returned
## otherwise the inverse is computed using the solve method and the inverse is
## set on the cacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } 
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
