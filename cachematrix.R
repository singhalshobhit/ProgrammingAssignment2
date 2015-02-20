## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## set function to set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x ## gets the value of the matrix
  setinverse <- function(solve) m <<- solve ## desired operation over the matrix, in this case solve
  getinverse <- function() m
  list(set = set, get = get, ##basic list ia returned. This is called by the next function
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { ## if m is not null, we get it from cache
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
