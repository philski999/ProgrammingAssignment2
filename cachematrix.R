## A module that provides a matrix object that caches its inverse

## Create a matrix that has a cached inverse
makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL # Cached inverse of the matrix
	set <- function(aMatrix) {
	  x <<- aMatrix
	  cachedInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) cachedInverse <<- inverse
	getInverse <- function() cachedInverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## return the inverse of a matrix, either by calculating (and then caching) the inverse
## or by returning an existing cached inverse. x is a cacheable matrix made with makeCacheMatrix
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return (inverse)
  }
  originalMatrix <- x$get()
  x$setInverse(solve(originalMatrix, ...))
  x$getInverse()
}
