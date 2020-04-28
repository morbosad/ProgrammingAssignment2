## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
