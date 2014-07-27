# This function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
# The input is received by x which expects a matrix
# The function "get" recieves the input from x
# The function "set" recieves y as input and sets it to x
# The function "setinverse" sets the inverse to the input it recieves and moves it to parameter m
# The function "getinverse" gets the inverse and returns m


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix.
# The function first checks if an inverse exists.
# If it does, then it returns the vale of m
# Alternatively, it computes the inverse using solve, sets it to variable m and returns m

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
