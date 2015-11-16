## functions for computing the inverse of matrices with cached results
## 
## example usage:
##   mat <- matrix(seq(1:4), 2, 2)
##   cache <- makeCacheMatrix(mat)
##   cacheSolve(mat) # computes the inverse and stores it
##   cacheSolve(mat) # returns the cached inverse without re-computing it

## create a cache for storing a matrix and its inverse
#
# @param    x the original matrix we want to cache
# @returns  the cached matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set our cache contents
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  
  # get our cache contents
  get <- function() x
  
  # set the inverse matrix
  setinverse <- function(inv) m <<- inv
  
  # get the inverse matrix
  getinverse <- function() m
  
  # API for our clients
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## compute the inverse of a matrix
#
#  @param x  a cache containing the source matrix; a new cache can be obtained using makeCacheMatrix() 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  # compute the inverse matrix
  m <- solve(data)
  # cache it
  x$setinverse(m)
  # return it
  m
}
