## TEMPLATE
## FUNCTIONS-
## makecachematrix-this function creates a defined matrix which can cache the inverse of it
## cachesolve- This fucntion computes the inverse if the matrix returned by the above makecachematrix and if the matrix is not inversed then it should retrieve the matrix from cache

makeCacheMatrix <- function(x = matrix()) {

  cacheMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  getMatrix <- function() x
  setCache <- function(inverse) cacheMatrix <<- inverse
  getCache <- function() cacheMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
}

cacheSolve <- function(x, ...) {
  
   cacheMatrix <- x$getCache()
  
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
}
}
