## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1. function "makeCacheMatrix" 
## provide setter and getter functions for matrix and inverse matirx

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(z) inverse <<- z
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function
## function "cacheSolve"
## Check if the inverse has already been cached.
## If cached, return the cached inverse matrix
## If not, caculate the inverse matrix using "solve"
## Then return the inverse and cache the caculated inverse

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    
    message("getting cached data")
    
    return(inverse)
    
  }
  
  data <- x$get()
  
  size <- nrow(data)
  
  inverse <- solve(data,diag(1,size,size))
  
  x$setinverse(inverse)
  
  inverse
  
}