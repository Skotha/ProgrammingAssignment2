## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(sk = matrix())  {
  skm <- NULL
  set <- function(y) {
    sk <<- y
    skm <<- NULL
  }
  get <- function() sk
  setinverse <- function(skz) skm <<- skz
  getinverse <- function() skm
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(skcache, ...) {
  skm <- skcache$getinverse()
  if(!is.null(skm)) {
    message("I am getting cached data")
    return(skm)
  }
  skdata <- skcache$get()
  skm <- solve(skdata)
  skcache$setinverse(skm)
  skm
}
