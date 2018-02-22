## The makeCacheMatrix function creates a matrix
## The cacheSsolve function will retrive a cached matrtix and return
## its inverse.

## Function creates a reversible matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function will create the inverse of cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
