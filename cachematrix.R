## Together, the two functions allow you to cache a matrix's calculated inverse
## so that you can simply call the inverse instead of recalculating it in the future

## makeCacheMatrix creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y 
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse1) inverse <<- inverse1
  
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the inputted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
