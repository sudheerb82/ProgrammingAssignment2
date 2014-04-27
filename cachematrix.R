
## These two functions put together enables caching of the inverse of a matrix. 
## Inverse will be calculated first time the cacheSolve function is called and next time cacheSolve
## is called, inverse will be returned from cache until matrix object is changed.


## makeCacheMatrix creates a speical "matrix" object - which basically is a list exposing 4 functions to :
## 1. set the original matrix which can be invertible
## 2. get the original matrix
## 3. set the inverse of the original matrix
## 4. get the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {

  ## set inverse to null
  inverse <- NULL
  
  ## function to store the value of original matrix and mark inverse as null
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  ## returns the original matrix
  get <- function() x
  
  ## function to set the value for the inverse
  setInverse <- function(inv) inverse <<- inv
  
  ## function to get the inverse from the cache
  getInverse <- function() inverse
  
  ## return the list object with 4 functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve takes input of the special "matrix" object created by makeCacheMatrix function
## and returns the inverse of the input matrix

cacheSolve <- function(x, ...) {

  ## first try to get inverse of matrix.
  inverse <- x$getInverse()
  
  ## if the inverse is not null, then return the inverse
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  ## get the original matrix through the special matrix object
  data <- x$get()
  
  ## compute the inverse using solve function
  inverse <- solve(data,...)
  
  ## set the inverse of the matrix in the special "matrix" object
  x$setInverse(inverse)
  
  ## return the inverse
  inverse
}
