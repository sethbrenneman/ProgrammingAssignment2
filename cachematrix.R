## makeCacheMatrix returns a list of functions: get, set, getInverse, and setInverse
##  given a matrix as an argument

## cacheSolve takes a makeCacheMatrix object.  If the matrix has already been solved,
##  it returns the cached inverse.  Otherwise, it will solve the inverse and save it to 
##  the cache

## given the matrix x, returns a list of functions for getting/setting the matrix and its
##  inverse.  Initially sets the inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL;
  getInverse <- function () inv
  get <- function () x
  setInverse <- function (i) inv <<- i
  set <- function(newM) {
    x <<- newM
    inverse <<- NULL
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## If x's inverse has already been solved, return the cached copy.  Otherwise, solves the
##  inverse and saves it to the cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matr <- x$get()
  i <- solve(matr)
  x$setInverse(i)
  i
}
