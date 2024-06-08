makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  
  set <- function(y) {
    x <<- y  # Assign the input matrix y to the variable x in the parent environment
    inv <<- NULL  # Reset the inverse property in the parent environment
  }
  
  get <- function() x  # Return the matrix x
  
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse property
  
  getInverse <- function() inv  # Get the inverse property
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  # Return a list of the above functions
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to get the inverse from the cache
  
  if (!is.null(inv)) {  # If the inverse is already cached, return it
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()  # Get the matrix from the special "matrix" object
  inv <- solve(mat, ...)  # Compute the inverse of the matrix
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}

