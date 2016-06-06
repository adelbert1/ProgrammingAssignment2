## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The following code creates a function called makeCacheMatrix that can be used
## to make a modified matrix object capable of storing the its inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL   
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inverse <<- inverse
      getInverse <- function() inverse
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This next function determines the inverse of the matrix in one of two ways: 
## 1) First, it checks to see if the inverse has already been computed and
## is not null. If this is the case, then it retrieves and returns the inverse
## that is stored in the cache.
## 2) Second, if the inverse has not yet been calculated and is determined to be
## null, the function computes the inverse and then sets the inverse in the. 
## makeCacheMatrix object.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      mat <- x$get()
      inverse <- solve(mat, ...)
      x$setInverse(inverse)
      inverse
}
