## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## The funciton will create a list that will set and get the value of matrix and then set and get 
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_val <- NULL ## custom
  
  set <- function(y) {
    x <<- y
    inverse_val <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inverse_val <<- inverse
  getinverse <- function() inverse_val
  
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
} # end of makeCacheMatrix funciton



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_val <- x$getinverse()
  
  
  # if confdition to check if the Matrix has now chaned, Cachesolve will return inverse from the cache
  if(!is.null(inverse_val)) {
    message("getting cached data.")
    return(inverse_val)
  } # end of if
  
  else # otherwise calculating the inverse
  {
    matrix_data <- x$get()
    inverse_val <- solve(matrix_data)
    x$setinverse(inverse_val)
    inverse_val
  } # end of Else
  
  
}
