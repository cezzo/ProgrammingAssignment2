## This pair of functions demonstrates how caching can be used to draw values calculated
## in other functions.

## This function creates a matrix and caches the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<- function(y) {
    x<<- y
    i<<-NULL
  } 
  get<-function() x
  setinverse<- function(solve) i<<- solve
  getinverse<- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

## This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then this function retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}

## example of how to use these functions:
  ##mtx<-makeCacheMatrix()
  ##mtx$set(matrix(21:24,2,2))
  ##cacheSolve(mtx)