## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Placeholder Inverse
  inv <- NULL
  
  ## Set the value of the matrix
  set <- function (y) {
    x <<- y
    inv <- NULL
  }
  
  ## Get the value of the vector
  get <- function() x
  
  ## Set the value of the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  ## Get the value of inverse
  getInverse <- function () inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Retrieve Inverse if it is already cached
  Inverse <- x$getInverse()
  
  ## If the cache is not NULL, return the cache
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  
  ## If the cache is NULL, calculate the Inverse and store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
