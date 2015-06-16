## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function


## this function are to create special square matrix and catch the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
} ##end makeCacheMatrix

## cacheSolve the inverse of matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) #single parameter 
  x$setinverse(inv)
  inv
} ##end cacheSolve
