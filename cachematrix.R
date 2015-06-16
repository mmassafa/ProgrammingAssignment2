## Caching the Inverse of a Matrix
## The following functions will: 
## 1. calculate the inverse of a matrix
## 2. save to cache
## 3. get value from cache OR calculate inverse

## makeCacheMatrix creates a special matrix 
## and sets and gets the values for the vector and for the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve will check if inverse has already been calculated 
## if yes: get inverse from cache
## if no: calculate inverse then set value of inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}