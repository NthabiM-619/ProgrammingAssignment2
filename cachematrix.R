## A set of functions to cache the inverse of a Matrix.
## functions do

## The following function creates a special "Matrix" object that can Cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y)  {
    X <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(invesre)  {
    inv <<- inverse 
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
  
}
    
##This following function computes the inverse of the special "Matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinvesre()
  if (!is.null(inverse))  {
    return(inverse)
  }
  m <- solve(x$get())
  x$setinverse(m)
  return(m)
}
