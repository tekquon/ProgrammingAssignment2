## Contains two functions to create a matrix which stores a cached inverse matrix
##    1. makeCacheMatrix: Creates the cache-able matrix
##    2. cacheSolve: Returns the inverse value of a matrix


## Creates a "cache-able" matrix, with ability store initial matrix and inverse matrix.
## Sets initial matrix to x and inverse matrix to null.
## Allows retrieval of matrix and inverse matrix: get(), getInverse()
## Allows changes to matrix and inverse matrix: set(), setInverse()
## Returns a list of functions: set(), get(), setInverse(), getInverse()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  ##Sets inverse to null
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Retrieves the inverse matrix of x.
## If cached in makeCacheMatrix, returns cached value.
## If not cached, calculates inverse matrix, stores in makeCacheMatrix, and returns inverse matrix.
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(x$get())
  x$setInverse(i)
  i
}
