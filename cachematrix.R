## This function creates an environment with cached values
## for a matrix and its inverse and creates mutator and 
## accessor methods for them.


makeCacheMatrix <- function(x = matrix()) {
  # Initiating the inverse as non existing
  I <- NULL
  
  # Mutator - resets the environment with new matrix
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  
  # Accessor - returns the matrix
  get <- function() x
  
  # Mutator - Computes new inverse
  setinverse <- function(solve) I <<- solve
  
  # Accessor - Returns 'cached' inverse
  getinverse <- function() I
  
  # Sets the methods of the 'MakeCacheMatrix' object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a 'MakeCacheMatrix' object and 
## returns the inverse of the cached matrix

cacheSolve <- function(x, ...) {
  ## Checks if the cached matrix is invertible
  if(det(x$get())!=0){
    
    ## Returns the cached inverse if already computed
    I <- x$getinverse()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    
    ## otherwise computes it, sets it and returns it
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
  } 
  ## Returns error message for non invertible matrices
  else {
    message("Matrix non invertible")
  }
}
