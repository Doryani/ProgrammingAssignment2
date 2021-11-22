## Following the example provided to a T really
## first function creates a special matrix that
## contains functions to set and get the inverse
## the second function checks the special matrix for a cached mean

## creates a special "matrix" with functions to set and get inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## checks the special matrix to see if it's inverse had been cached prior

cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
  if (!is.null(inv)) {
          message("Scanning for cached inverse...")
          return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
