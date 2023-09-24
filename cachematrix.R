## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# First we create a function. This function's input is a matrix and the output is a list
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function gets the output of the `makeCacheMatrix` function and returns the inverse of the original matrix.
# If the inverse was already calcualted, than it is retrieved from the cache (and the user is informed about it).
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
