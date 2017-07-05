## Put comments here that give an overall description of what your
## functions do
## Functions makeCacheMatrix and cacheSolve works together and one without the second one do not have much sense. 
## Both together creates environment a handler for caching inverse of the given matrix.
## 
## 
##

## Write a short comment describing this function
## The function makeCacheMatrix creates list which stores functions which allows access for getting and setting matrix and invers of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x 
  setmatrix <- function(solve) m_inv <<- solve
  getmatrix <- function() m_inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## Write a short comment describing this function
## The function cacheSolve return inverse of the matrix
## The function cacheSolve uses structure created in makeCacheMatrix. 
## In the begining check if the inverse matrix exist in the structure created in makeCacheMatrix. 
## If yes than uses cached inverse and return it and notify user that the cached value was used.
## If cached value do not exist function calculates it, 
## stores it in the structure created by makeCacheMatrix and return it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getmatrix()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setmatrix(m_inv)
  m_inv
  
  }
