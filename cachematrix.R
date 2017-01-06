## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##a function that creates and caches
##its inverse - uses get and set functions
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv_matrix <<- inverse
    getInverse <- function() inv_matrix
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## Write a short comment describing this function
##function that takes a matrix created by makeCacheMatrix
##and returns the inverse
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  ##use getInverse to get the inverse
  inv_matrix <- x$getInverse()
  ##if it has already been cached return the cache
  if (!is.null(inv_matrix)) {
    message("has been cached - getting cached data")
    return(inv_matrix)
  }
  ##otherwise get the matrix and 
  ##do the inversion and return that
  orig_matrix <- x$get()
  inv_matrix <- solve(orig_matrix, ...)
  ##set the inverse
  x$setInverse(inv_matrix)
  ##return it
  inv_matrix
}
