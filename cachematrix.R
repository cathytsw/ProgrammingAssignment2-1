## A pair of functions for caching the inverse of square invertible matrix

## This function creates a special object that stores a matirx and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) inverse <<- solve
  getinverse<-function() inverse
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This fucntion computes the inverse of the matrix created by the above makeCacheMatrix function.
## If the inverse of  matrix has already been computed and the matrix have not been changed, 
## this function retrieves the inverse of matrix from the cache. 
## Otherwise, this function will compute the inverse of matrix and set that inverse in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
