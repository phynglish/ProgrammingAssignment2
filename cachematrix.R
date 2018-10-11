## The functions here can be used to speed up calculating the inverses of
## large numbers of matrices by checking if the inverses have already been
## calculated and stored in the cache.

## The function makeCacheMatrix takes in a matrix and prepares it as a
## list for use in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve first checks whether the inverse of the matrix
## has already been calculated. If it has, it returns the inverse from the cache.
## Otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
