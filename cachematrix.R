## The result of the below function is the list of four functions, which task is to
## set and get the value of matrix and set and get the value of its inverse matrix.
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


## Function "cacheMatrix" calculates the value of inverse matrix, 
## which is the object given by funcion "makeCacheMatrix".
## If the inverse matrix hasn't been calculated the function does calculatios 
## and gives the result.
## If calculation has already been done, the result of that function will be information:
## 'getting cached data'.
## Assumption: input matrix is invertible.

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
