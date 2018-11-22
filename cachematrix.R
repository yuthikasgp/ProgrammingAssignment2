## These functions caches a matrix and computes and saves it's inverse matrix
## 

## The makeCacheMatrix function returns a list containing four functions
## These four functions are described below
## getmatrix : returns the cached matrix
## setmatrix : reset the cached matrix
## getinverse : returns the cached inverse of the original matrix
## setmatrix : reset the cached inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  
  setmatrix <- function(y){
    x <<- y    
    x_inverse <<- NULL
  }
  
  getmatrix <- function() {    
    x
  }
  
  getinverse <- function() {
    x_inverse
  }
  
  setinverse <- function(inv) {
    x_inverse <<- inv
  } 
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns the cached inverse matrix if it has already been calculated
## otherwise, it calculates the inverse using the solve() method, and caches this inverse using the setinverse function 
## and returns that inverse

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
