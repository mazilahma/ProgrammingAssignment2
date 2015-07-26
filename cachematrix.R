## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  ##set the matrix
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  ##get the matrix
  get <- function() x
  ##set the inverse
  setinverse <- function(inverse) inv <<- inverse
  ##get the inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  ##if cache is exist
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  ##get the matrix from the object
  data <- x$get()
  ##solve the inverse
  inv <- solve(data, ...)
  ##set the inverse to the object
  x$setinverse(inv)
  inv
}
