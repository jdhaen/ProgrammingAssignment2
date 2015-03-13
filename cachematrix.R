# As matrix inversion is a costly calculation, we create functions
# to cache the result of an inversion and retrieve it. This prevents
# the computation of the inversion if it already exists.

## The function below creates the "special" cache matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # A function called set is created. It creates the cached value x
  # and sets the cached value m to NULL
  # Note: <<- assigns a value to a parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Create a function to return the input matrix
  get <- function() x
  
  # Computing the inverse
  setinverse <- function(inverse) m <<- inverse
  # Retrieving the inverse
  getinverse <- function() m
  
  # Return a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function below calculates the inversion of the cache matrix
## create by makeCacheMatrix. If the inversion has been calculated
## before, it retrieves the cached version.

cacheSolve <- function(x, ...) {
   # Get the inverse...
  m <- x$getinverse()
  # ... if the inverse was already calculated before,
  # m will NOT be null, thus cached value can be retrieved
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If the inverse has not been calculated yet, 
  # it will be calculated now
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
