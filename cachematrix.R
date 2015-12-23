## The first function, makeCacheMatrix, takes as input a square
## invertible matrix and outputs a list that caches that matrix.

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

## The second function, cacheSolve, takes as input the output of
## makeCacheMatrix, and outputs the inverse of the original matrix.
## If the inverse has already been calculated, it gets it from 
## the cache and skips the calculation.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m
}
