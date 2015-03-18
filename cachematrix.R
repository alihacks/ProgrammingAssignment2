## Set of functions to inverse matrices and cache for better performance
## Coded by alisoylu@gmail.com

## Creates a matrix object that can be cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { # sets out matrix, clears out cached inverse
    x <<- y
    m <<- NULL
  }
  get <- function() x # get the matrix
  
  setinv <- function(solve) m <<- solve # inverse and store result
  getinv <- function() m # get inverse that we stored
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## inverses a matrix, if the matrix is already cached, returns cached result

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
