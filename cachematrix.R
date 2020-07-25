# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly (there are also alternatives to matrix inversion that we will
# not discuss here). Your assignment is to write a pair of functions that
# cache the inverse of a matrix.  For this assignment, assume that the matrix 
# supplied is always invertible.

## example usage
## example_matrix <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(example_matrix)
## cacheSolve(example_matrix)  # second time will show cached
## example_matrix2 <- makeCacheMatrix(matrix(rexp(1000000),1000))
## cacheSolve(example_matrix2)
## cacheSolve(example_matrix2) # second time will show cached


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
#  already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the `solve`
# function in R. For example, if `X` is a square invertible matrix, then
# `solve(X)` returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}





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