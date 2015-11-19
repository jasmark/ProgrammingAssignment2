## functions to implement a cached copy of a matrix and its inverse to prevent
## from needing to re-solve the matrix if already solved previously

## makeCacheMatrix 
## @x: an invertible square matrix
## return list of functions that enable get/set for the matrix 
## and the inverse of the matrix. Used by the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  
  m_inverse = NULL
  
  # function to store the matrix in another environment to enable caching
  set = function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  
  # function to  get function returns the matrix
  get = function() x
  
  # function to set the inverse of the matrix in the cache
  setinv = function(inverse) m_inverse <<- inverse
  
  # function to return the inverse matrix
  getinv = function() m_inverse
  
  # return the list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve
## @x: a matrix output by makeCacheMatrix
## return: inverse of original matrix, either cached copy or newly solved if not 
## previously calculated
cacheSolve <- function(x, ...) {
  
  # return inverse, check to see cached copy
  m_inverse = x$getinv()
  if(!is.null(m_inverse)) {
    message("getting cached matrix inverse")
    return(m_inverse)
  }
  
  # if not a cached copy, solve the matrix, store in the cache, and return the inverse
  data <- x$get()
  m_inverse = solve(data, ...)
  x$setinv(m_inverse)
  
  m_inverse        
}
