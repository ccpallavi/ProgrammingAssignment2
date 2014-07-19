## These pair of functions are to cache the results of Matrix inversion.
## Matrix inversion is a costly compuation. Hence, in order to improve the performace, 
## we can cache the result, and use the cached result instead of computing the 
## inverse of the matrix each and every time.



## The function makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  ## sets the matrix to the given value, and inverse matrix to NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  ## get function returns the matrix
  get <- function() x
  
  ## stores the matric inversion value
  setInverseMartix <- function(i) inverseMatrix <<- i
  
  ## returns the martix inversion value
  getInverseMartix <- function() inverseMatrix
  
  ## returns the list of all needed functions
  list(set = set, get = get,
       setInverseMartix = setInverseMartix,
       getInverseMartix = getInverseMartix)  
}




## The function cacheSolve calculates the inversion of the special "martix" 
## created with makeCacheMatrix function. 
## Logic - If the matrix inversion has already been calculated, it gets the martix
## inversion from the cache and skips the computation. Else, it computes the value 
## and stores in the cache.

cacheSolve <- function(x, ...) {
  
  ## gets the cached inverse martix
  inverseMatrix <- x$getInverseMartix()
  
  ## if cached inverse matrix is not null => return the cached value
  if(!is.null(inverseMatrix)) {
    message("Getting cached inverse matrix")
    return(inverseMatrix)
  }
  
  ## if the cachede inverse matrix is null => compute the value
  message("Computing inverse matrix")
  matrix <- x$get()
  inverseMatrix <- solve(matrix, ...)
  
  ## store the computed value in the cache
  x$setInverseMartix(inverseMatrix)
  
  ## return the computed value
  inverseMatrix
}
