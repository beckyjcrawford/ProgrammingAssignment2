## The following two functions implement a cached matrix that can cache
## its inverse and a cached version of solve(...) that takes the cached
## matrix and stores a cached copy of the matrix inversion the first 
## time it is called. It then returns the cached copy on later calls.

## Write a short comment describing this function
# Create the matrix object with a cache for the inverse
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) invmat <<- matinv
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
#Cached version of solve(...) using a cacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinv()
    if(!is.null(invmat)) {
      message("getting cached data")
      return(invmat)
    }
    data <- x$get()
    invmat <- solve(data, ...)
    x$setinv(invmat)
    invmat
}