## This code performs similar to the 
# code given as example

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse<- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}  
  
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
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