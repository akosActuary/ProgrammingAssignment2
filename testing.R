# testing the functions in the instructions

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

x_1 <- rnorm(10)

z <- makeVector(x = x_1)

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
cachemean(z)

x_2 <- rnorm(15)
z_2 <- makeVector(x_2)

cachemean(z_2)
cachemean(z)

### Trying it with a matrix

# test matrix
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
A_inverse <- solve(A)
# gives back the ID mtx
A %*% A_inverse
