## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## THis function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # mat and inv are the two local variables of this function
  mat <- x
  inv <- NULL
  
  get <- function()mat 
  
  set <- function(y){
    mat <<- y
    inv <<- NULL
  }
  
  getInverse <- function()inv
  
  
  setInverse <- function(invparam){  
    inv <<-invparam
  }
  
  list(get=get, set=set, getInverse = getInverse, setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invM <- x$getInverse()
  
  if(!is.null(invM)){
    
    message("geting cached data.....")
    return (invM)  
  }
  mat <- x$get()
  invM <- solve(mat,...)
  x$setInverse(invM)
  invM
}
