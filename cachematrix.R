## This file contains function to calculate and cache inversion of the matrix
## It is done as part of Coursera R Programming course

# Create new CacheMatrix object with following functions:
#   set(new.value)   - set new matrix value, clear cached inversion
#   get()            - get stored matrix
#   set.inv(new.inv) - set new cached inversion of the matrix
#   get.env()        - get stored inversion of the matrix
#
# Args:
#   x - matrix object used to create new CacheMatrix
#       default: empty matrix
#
# Returns:
#   new CacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
  matrix.inv <- NULL
  set <- function(new.value) {
    x <<- new.value
    matrix.inv <<- NULL
  }
  get <- function() x
  set.inv <- function(new.inv) matrix.inv <<- new.inv
  get.inv <- function() matrix.inv
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}


# Calculate inversion of the matrix. It use cache if avaliable.
#
# Args:
#   x   - matrix, created by makeCacheMatrix
#   ... - function pass extra argument to solve function
#
# Returns:
#   inversion of the matrix
cacheSolve <- function(x, ...) {
  x.inv <- x$get.inv()
  if(!is.null(x.inv)) {
    message("getting cached data")
    return(x.inv)
  }
  x.inv <- solve(x$get(), ...)
  x$set.inv(x.inv)
  x.inv
}

# This function is just simple test
test <- function() {
  B <- matrix(c(4, 3, 3, 2), nrow=2, ncol=2)
  x <- makeCacheMatrix(B)
  print(cacheSolve(x) %*% B)
  print(cacheSolve(x) %*% B)
  C <- matrix(c(1, 2, 3, 5), nrow=2, ncol=2)
  x$set(C)
  print(cacheSolve(x) %*% C)
  print(cacheSolve(x) %*% C)
  x$set(B)
  print(cacheSolve(x) %*% B)
  print(cacheSolve(x) %*% B)
}