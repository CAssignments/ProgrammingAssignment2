## makeCacheMatrix and cacheSolve serve the purpose of letting the user work with matrices 
##   that cache the result of the solve function for better performance.
##   First you need to create the special cache-matrix object, by calling makeCacheMatrix and 
##   passing your matrix as an argument.
##   After that, whenever you want the inverse of this matrix, just call the cacheSolve function,  
##   passing your special cache-matrix object as an argument.


## makeCacheMatrix - This function returns an object which is really a list of 4 functions and 
##                   has a matrix and its inverse in its environment. The inverse isn't 
##                   calculated until it's needed and the result is then cached (until 
##                   the matrix changes) for faster retreival.
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the variable that will store the cached inverse.
  mat_inv <- NULL
  
  # Function to change the matrix for this object.
  set <- function(new_mat) {
    x <<- new_mat # Store the new matrix value.
    mat_inv <<- NULL # Set the inverse to NULL to indicate it needs to be recalculated.
  }
  
  # Returns the matrix for this object.
  get <- function() x
  
  # Store the inverse of this object.
  setinverse <- function(inv) mat_inv <<- inv
  
  # Returns the cached inverse for this object.
  getinverse <- function() mat_inv
  
  # This line creates a list object where it stores the 4 functions defined above.
  # Since this is the last line of the makeCacheMatrix, this list is also returned.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve - Returns the inverse of the matrix associated with the object x
##              and caches the result for future calls. If the function was
##              already called, the cached result will be returned (unless the 
##              matrix was changed, in which case it will first be recalculated).
cacheSolve <- function(x, ...) {
  # Fetch the cached inverse.
  inv <- x$getinverse()
  
  # If the cached inverse is not NULL, it means we've already calculated it.
  # In this case, we'll just return the cached result.
  if(!is.null(inv)) {
    return(inv)
  }
  
  # If we got this far, it means we haven't calculated the inverse after 
  # the matrix was set/changed last time, so we need to do that now.

  # Fetch the actual matrix from the special object that was passed.
  mat <- x$get()

  # Run solve on the matrix to calculate the inverse. (The ... are passed along 
  # from the parameters passed to the cacheSolve function.)
  inv <- solve(mat, ...)
  
  # Cache the result of our calculation so we can retrieve it the next time this
  # function is called.
  x$setinverse(inv)
  
  # Finally, return the inverse we calculated.
  inv
}
