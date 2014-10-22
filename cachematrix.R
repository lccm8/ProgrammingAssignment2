## cachematrix.R caches the inverse of a matrix for future use by applying
## concepts of lexical scoping

## makeCacheMatrix() creates a vector containing functions to set and return the
## original matrix and also its inverse

makeCacheMatrix <- function(m) {
  im <- NULL
  set <- function(p) {
    m <<- p
    im <<- NULL
  }
  get <- function() m
  setinverse <- function(inv) im <<- inv 
  getinverse <- function() im
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## CacheSolve() tries to get the inverse of the matrix cached before calculating
## it

cacheSolve <- function(m) {
  im <- m$getinverse()
  if(!is.null(im)) {
    print("Getting cached data")
    return(im)
  }
  data <- m$get()
  im <- solve(data)
  m$setinverse(im)
  im
  
}
