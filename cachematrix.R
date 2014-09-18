## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(sk = matrix())  {
  skm <- NULL
  set <- function(y) {
    sk <<- y
    skm <<- NULL
  }
  
  ## provide other utility functions for the matrixinverse
  ## return the matrix value
  get <- function() sk
  
  ## set the inverse of the matrix
  setinverse <- function(skz) skm <<- skz
  
  ## get the inverse of the matrix
  
  getinverse <- function() skm
  
  ## retun the list of functions supported.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(skcache, ...) {

## get the inverse first. it is true, no need to do the inverse again.
  skm <- skcache$getinverse()
  if(!is.null(skm)) {
    message("I am getting cached data")
    return(skm)
  }
  
  ## nothing in the cache...do the inverse and set it to the cache.
  skdata <- skcache$get()
  skm <- solve(skdata)
  skcache$setinverse(skm)
  skm
}
