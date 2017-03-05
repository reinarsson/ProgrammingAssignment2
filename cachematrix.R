## Together these functions provice a way to cache the inverse of a matrix

## This function creates a list that contains 4 functions:
## 1) Set a matrix
## 2) Get a matrix
## 3) Set the inverse of a matrix
## 4) Get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <- NULL
  }
  get <- function() x
  setInv <- function(invM) m <<- invM
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns a matrix that is the inverse of x.
## If the inverse has already been cached it will be returned, otherwise the inverse is calculated.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
