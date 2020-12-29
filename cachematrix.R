makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cachesolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}

source("makeCacheMatrix.R")
matrix <- makeCacheMatrix(matrix(1:16, nrow = 4, ncol = 4))
matrix$get()
matrix$getInverse()
cachesolve(matrix)
