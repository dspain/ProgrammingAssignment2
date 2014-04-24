## These functions create a special matrix object that is capable of
##  computing its own inverse.  It will grab the inverse from the cache
##  if it has already been caculated.

## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    s <<- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Computes the inverse of the matrix returned by makeCacheMatrix.
##  Retrieves the inverse from cache if it has already been calculated.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
