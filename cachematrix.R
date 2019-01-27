## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # set function resets matrix & resets inv to NULL in parent environment
    x <<- y # resets the value of x to y in the parent environment
    inv <<- NULL # resets value of inv to NULL in parent environment
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting the cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) # find 1/x
  x$setInverse(inv) 
  inv
}
