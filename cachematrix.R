## Check whether the matrix has been cache. If not cache it, otherwise retrieve previous 
## calculation.

## create a cache for the the new matrix. It will return the previous saved calculation.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This is to check whether the matrix calculation has been cached. Data that has not been cached before will proceed
## with the calculation and finally cache using the MakeCache function for next retrieval. If the calculation
## has been cached before then it will simply get the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
