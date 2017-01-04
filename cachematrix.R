## calculate the inverse of a matrix once then cashing it without the need
## to calculate it again

## this function constructs a matrix and return a list with four fields 
## to set or get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) x_inv <<- inv
  getinv <- function() x_inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this function return the inverse of a matrix
## and compute it if it is not computed yet

cacheSolve <- function(x, ...) {
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached inverse")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}
