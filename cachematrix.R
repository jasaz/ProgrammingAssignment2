## Matrix inversion is usually a costly computation and there is some benefit to caching the inverse of a matrix rather than compute it repeatedly

## THis function creates a matrix object that can cache its inverse. It reruns a list of functions.
## <<- operator returns the object of an environment different from it current environment (parent environment).

makeCacheMatrix <- function(x = matrix()) {
  
  # mat and inv are the two local variables of this function
  mat <- x
  inv <- NULL
  
  get <- function() mat 
  
  
  set <- function(y){
      mat <<- y
      inv <<- NULL          ## Everytime the matrix mat is set, it will automatically set the inverse inv to NULL
  }
  
  getInverse <- function()inv
   
  setInverse <- function(invparam){  
      inv <<-invparam
  }
  
  list(get=get, set=set, getInverse = getInverse, setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve will retrieve the inverse from the cache. If the matrix has changed then it will compute inverse once again.

cacheSolve <- function(x, ...) {
  
  invM <- x$getInverse()
  mat <- x$get()
  
  if(!is.null(invM)){
    
      message("geting cached data.....")
      return (invM)  
  }
  mat <- x$get()
  
  invM <- solve(mat,...)
  
  x$setInverse(invM)
  
  invM
}
