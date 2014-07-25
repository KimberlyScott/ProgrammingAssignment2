## Computes and caches the inverse of a matrix to avoid the
## computational expense of inverting a matrix if it has 
## already been computed

## makeCacheMatrix - create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invx <<- solve
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Compute the inverse of the matrix created in makeCacheMatrix by
## either 1) computing it directly or 2) retrieving it from cache
## if it's already been computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}