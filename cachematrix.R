## This R file includes functions related to creation
##     of matrices which can store (or cache) inverted 
##     versions and calculation of this inversion
##     considering this matrix class

## makeCacheMatrix funtion creates a matrix object that 
##     can store inverted version of itself

makeCacheMatrix <- function(x = matrix()) {
  
  # Set initial values
  inv <- NULL
  ifchanged <- TRUE
  
  # Create set/get functions
  set <- function(y) {
    x <<- y
    inv <<- NULL
    ifchanged <<- TRUE    
    # If new values entered, change ifchanged flag
  }
  get <- function() x
  setinv <- function(invVal) inv <<- invVal
  getinv <- function() inv
  setifch <- function(ifchangedVal) ifchanged <<- ifchangedVal
  getifch <- function() ifchanged
  
  # Return list of set/get functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv,
       setifch = setifch,
       getifch = getifch)
}


## cacheSolve is a function that can compute the inverse
##     of a matrix, or return the inverse value of a matrix
##     if it is stored in the input matrix object

cacheSolve <- function(x, ...) {
        
  # Check if inverse exist and matrix didn't change
  inv <- x$getinv()
  ifchanged <- x$getifch()
  if(!is.null(inv) && !ifchanged) {
    message("getting cached data")
    return(inv)
  }
  
  # Calculate the inverse of the matrix if it isn't cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  x$setifc(FALSE)   # Change the flag after calculation
  
  # Return inverse of the matrix
  inv
}
