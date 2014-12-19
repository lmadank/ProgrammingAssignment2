## Routines to Cache the Inverse of a Matrix
## Author: LMADANK

## Function which creates the object which holds matrix 
## Invokes the "cacheSolve" routine to obtain the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will attempt to find the inverse of the given matrix passed
## and cache it in the global environment.
## Should the inverse be already available, it will be retrieved from the cache and 
## passed on to the calling function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
