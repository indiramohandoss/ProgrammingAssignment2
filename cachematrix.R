
  ## There are two functions in this script to create a special "matrix" object that can cache its inverse.
  ## First function creates a special list which returns functions to set & get a matrix, set & get the inverse of a matrix
  ## Second function calculates the inverse of a matrix if not already available and caches it, so that subsequent calls to get the value of the matrix inverse is returned from cache and not computed all over again
  
  ## Gets and sets the inversion of a matrix if already available
  
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves the inverse from the cache


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