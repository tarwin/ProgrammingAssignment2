## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  mat <- m
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## takes an object of type `makeCacheMatrix` and returns the inverse
## if the inverse has already been calculated then return cached inverse

cacheSolve <- function(m, ...) {
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'm'
  inv
}