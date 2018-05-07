makeCacheMatrix <- function(y = matrix()) {
  inv <- NULL
  set <- function(z) {
    y <<- z
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)  inv <<- inverse 
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(y, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- y$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- y$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}