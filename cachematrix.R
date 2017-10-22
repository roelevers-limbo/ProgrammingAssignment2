## Coursera R Programming Week 3
## Programming assignment 2

## Functions below can inverse a matrix and cache the resulting inversed matrix
## Usage: create a square matrix and pass it to the makeCacheMatrix
## The resulting list can be passed to tje cacheSolve function which inverses
## the matrix and returns the result of that computation. Subsequent calls to the function 
## will display a message informing you that the data is retrieved from cache and return the same inversed
## matrix

## makeCacehMatrix creates a special "matrix", which is really a list containing a function to

# * set the value of the matrix
# * get the value of the matrix
# * set the value of the matrix
# * get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mtrx <<- inv
  getinv <- function() mtrx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## CacheSolve inverses a given matrix ( use makeCacheMatrix to create the input for this function )

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
