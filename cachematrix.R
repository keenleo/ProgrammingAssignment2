## These functions calculate and cache the inverse of a matrix

## makeCacheMatrix creates vectors that can store the inverse of a matrix
## Usage
## Create Cache Matrix
##    foo <- makeCacheMatrix()
## Set Matrix B is a inversable matrix
##    foo$setmtrx(B)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmtrx <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmtrx <- function() x
  setinverse <- function(x) m <<- solve(x)
  getinverse <- function() m
  list(setmtrx = setmtrx, getmtrx = getmtrx,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will retrive the inverse of a matrix from cache, if it does not exist it will calculate the inverse of a matrix
## Usage
##    cacheSolve(foo)
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmtrx()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
