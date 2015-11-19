## Put comments here that give an overall description of what your
## functions do

##this function creates a special matrix that able to set the value of matrix, 
##get the value of matrix, set the value of inverse matrix and
##get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##initializing a matrix that is inverse of 'x'
  inv <- NULL
  
  ##setting the value of given matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##getting the value of given matrix
  get <- function() x
  
  ##inversing the given matrix and getting the result in two new variables
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  
  ##returning output
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

##this function computes the inverse returning by makeCacheMatrix function
##and if this iverse has already been calculated it extracts it from cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  
  ##checking if inverse matrix has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
