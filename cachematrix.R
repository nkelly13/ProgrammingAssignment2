## cachematrix.R contains two functions designed to offer cached matrix inversions. One function 

## Write a short comment describing this function
## makeCacheMatrix returns a list which acts as a matrix with a cacheable inverse. 
##  It's initialized from an input matrix, and offers getters and setters for the matrix, 
##  and its inverse.

## WARNING: calling setInverse inappropriately can cause this matrix-like list to appear to have
##  an incorrect inverse
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solution) matrixInverse <<- solution
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve returns a matrix with the inverse of x. If there's already a solution in the 
##  cacheMatrix's cache, that solution will be returned. If there's no cached solution, a new one
##  will be computed and cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv <- x$getInv()
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data, ...)
  x$setInv(matInv)
  matInv
}



