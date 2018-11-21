## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  
  setmatrix <- function(y){
    x <<- y    
    x_inverse <<- NULL
  }
  
  getmatrix <- function() {    
    x
  }
  
  setinverse <- function(inv) x_inverse <<- inv
  
  getinverse <- function() x_inverse
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if ( ! is.null(m)){
    message("getting cached inverse")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data)
  x$setinverse(m)
}
