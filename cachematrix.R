## Jason Michelizzi
## 27 Apr 2020
## 
## functions to implement a cached inverse matrix

## Makes an object that can cache its own inverse
makeCacheMatrix <- function(x = matrix()) {
  my_inv <- NULL
  
  get <- function () {x}
  set <- function (y) {
    x <<- y
    my_inv <<- NULL
  }
  getinverse <- function() {my_inv}
  setinverse <- function(inverse) {my_inv <<- inverse}
  
  list (get = get, 
        set = set, 
        getinverse = getinverse, 
        setinverse = setinverse)
}


## Solves for the inverse of the matrix and caches the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  raw_matrix <- x$get()
  inv <- solve (raw_matrix, ...)
  x$setinverse(inv)
  return(inv)
}
