##The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse<- function() m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    }  
  
## This functions returns inverse either from cache or calculates. 
# It first checks if the inverse has been calculated or not. If yes
# it returns that matrix. Otherwise, it calculates inverse and saves.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m) # this returns a matrix from cache
    }
    data <- x$get()
    m <- solve(data, ...) # calculates inverse if it is not in cache
    x$setInverse(m) # saves this in cache
    m # prints inverse
}
