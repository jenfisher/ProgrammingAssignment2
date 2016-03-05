## 





## makeCacheMatrix #####################################

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y)
  {
    
    x <<- y
    
    m <<- NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve ##########################################

## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## Computing the inverse of a square matrix can be done 
## with the solve function in R. For example, if X is a 
## square invertible matrix, then solve(X) returns its 
## inverse.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## Assume that the matrix supplied is always invertible
  
  m <- x$getinverse()
  
  
  ## return the cached data if the cache is not empty
  if(!is.null(m)) {
    
    return(m)
    
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  
  m
  
}
