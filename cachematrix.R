## Annelise Lynch Intro to R Assignment 2
## These functions cache the inverse of a matrix
## Assumption: The input will always be an inversible matrix

## The first function creates a matrix that gets and sets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list<-list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function calculates the inverse of the matrixcreated with the first function makeCacheMatrix
## Before calculating the inverse, ti checks to see if the inverse has already been calculated
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)%%data
  x$setinverse(m)
  m
}
