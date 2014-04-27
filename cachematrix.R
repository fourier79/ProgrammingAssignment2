## R Assignment 2

# the function below sets the input matrix and returns a list of functions
# the functions in the list are set, get, setInverse, and getInverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)    
}


# this function returns a matrix that is the inverse of 'x'
# it first checks whether the inverse is already computed, if so, reads it from cache
# otherwise, the inverse is computed by using "solve" function and it is stored for future use and returned
cacheSolve <- function(x, ...) {
        ## 
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
