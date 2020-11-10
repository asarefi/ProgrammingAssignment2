## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# we suppose y is a free  matrix variable and cached by x. m is an empty variable
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the cached matrix x. set the inverse of the matrix and cached the inverse of the matrix x to the variable m.
  # get the cached inverse of the matrix.
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setnverse = setnverse,
       getnverse = getnverse)
}


## Write a short comment describing this function
# 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # checks to see if m is an empty variable and if not returns the content of m; which is the cached inverse of x.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if the above operation evaluate to false, then the below operation gets the cached matrix 'x' and inverse it. 
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
