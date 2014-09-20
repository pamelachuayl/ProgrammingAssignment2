## There are two functions in this script, which are meant to help cache 
## the inverse of a matrix as matrix inversion is usually a costly operation 

# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Gets the value of the matrix
  get <- function() x
  # Saves the inverse of the matrix to variable 'm' as cache
  setinverse <- function(inverse) m <<- inverse
  # Gets the inverse of matrix from cache
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Computes the inverse of the matrix. If it has already been computed, then
# it will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # If the inverse is already cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If inverse not cached, then solve for it and save to cache variable
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
