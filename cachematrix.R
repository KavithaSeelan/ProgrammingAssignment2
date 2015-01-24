## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function caches the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setm <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getm <- function() x
  setinvm <- function(inverse) inv <<- inverse
  getinvm <- function() inv
  
  list(setm = setm, getm = getm,
       setinvm = setinvm,
       getinvm = getinvm)
  

}


## Write a short comment describing this function
## This function checks whether the inverse function is cache . 
## If available in cache then returns the cached information.
## If not available performs the inverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvm()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getm()
  inv <- solve(data, ...)
  x$setinvm(inv)
  inv
  
  
}
