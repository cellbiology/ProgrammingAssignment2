##These two functions cache the inverse of a matrix.
##We save time looking the inverse up in a cache,
##this eliminates the need to recompute the inverse.

#this function creates "matrix" object that caches inverse
makeCacheMatrix <- function(x= matrix ()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
  get <- function() x 
  setinverse <- function(solve)m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#this function computes/retrieves inverse of "matrix" object from makeCacheMatrix
cacheSolve <- function(x=matrix(), ...) {
  m <-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m 
}
