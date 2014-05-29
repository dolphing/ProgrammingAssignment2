## These two functions can calculate and cache the inverse of a matrix

## This function create a special "matrix", which is really a list containing
## functions to set or get the matrix, set or get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get the matrix
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv #get the cached inverse
  #create a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function returns the inverse of a matrix
## It gets and returns the inverse if there is a cache inverse of the matrix
## It calculates and returns the inverse if there is not a cache inverse
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  #get the cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  #calculate the inverse
  inv <- solve(data, ...)
  x$setinv(inv)
  inv

}

