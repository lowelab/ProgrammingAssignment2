## These functions can calculate the inverse of a matrix, and save the result to cache
## If the result is already in the cache, it will return the cached value not recompute

## This is a set of functions that are used by cacheSolve 
##    to determine if a cache value exists and return the value if it is cached.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return the inverse of matrix 'x'.
## If the inverse already exists in cache, return that, 
##    otherwise compute and set this result in the cache (and return value).

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}