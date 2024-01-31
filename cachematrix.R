# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Setter function to set the matrix
  setMatrix <- function(matrix) {
    x <<- matrix
    inv <<- NULL  # Invalidate the cached inverse
  }
  
  # Getter function to get the matrix
  getMatrix <- function() {
    x
  }
  
  # Setter function to set the inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Getter function to get the inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" and cache it
cacheSolve <- function(cacheMatrix) {
  # Retrieve the cached inverse
  inv <- cacheMatrix$getInverse()
  
  # If the inverse is already computed, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, compute it using solve
  mat <- cacheMatrix$getMatrix()
  inv <- solve(mat)
  
  # Cache the computed inverse
  cacheMatrix$setInverse(inv)
  
  inv
}

# Example usage:
# Create a special matrix object
matObj <- makeCacheMatrix(matrix(c(4, 2, 1, 1, 7, 5, 2, 5, 11), nrow = 3, byrow = TRUE))

# Retrieve and print the original matrix
originalMatrix <- matObj$getMatrix()
print("Original Matrix:")
print(originalMatrix)

# Compute and cache the inverse
cachedInverse <- cacheSolve(matObj)
print("Computed Inverse:")
print(cachedInverse)

# Retrieve the cached inverse
cachedInverse <- cacheSolve(matObj)
print("Retrieved Cached Inverse:")
print(cachedInverse)
