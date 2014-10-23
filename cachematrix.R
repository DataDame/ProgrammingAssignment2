## Functions makeCacheMatrix and cacheSolve compute the inverse of a matrix and cache the 
## inverse. Computing the inverse of a matrix can be a costly operation and it can be 
## beneficial to cache the result and return the cached result

## makeCacheMatrix returns a list of functions to get and set the matrix as well
## as to get and set the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatinv <- function(matinv) inv <<- matinv
  getmatinv <- function() inv
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}


## cacheSolve returns the inverse of a matrix if it has been cached else computes
## the inverse using solve, caches and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- tryCatch({solve(data)},
                    error = function(err) { 
                      message("Matrix inverse cannot be computed") 
                    })
  x$setmatinv(inv)
  inv
}
