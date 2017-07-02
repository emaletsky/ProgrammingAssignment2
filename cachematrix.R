## Put comments here that give an overall description of what your
## functions do

## Given a matrix create a matrix that has cache attached to it
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Computes inverse of a matrix created by makeCacheMatrix
## If x contains cached inverse - the cached value will be returned 
## Othervise inverse will be calculated, stored in cache and returned

cacheSolve <- function(x, ...) {
  cached <- x$getInverse()
  if(!is.null(cached)) {
    message("getting cached data")
    return(cached)
  }
  data <- x$get()
  inverse <- inv(data, ...)
  x$setmean(inverse)
  inverse
}
