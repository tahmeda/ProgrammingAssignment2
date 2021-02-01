## Put comments here that give an overall description of what your
## functions do
## Tahmeda Munim

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special matrix object that can cache its inverse
    inv <- NULL
    set <- function(y){
      x <<- y 
      inv <<- NULL
      
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve should retrieve the inverse from the cache.
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
