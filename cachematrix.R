## We need to make two functions

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## This function makeCacheMatrix gets a matrix as an input
#set the value of the matrix,
#get the value of the matrix, set the inverse Matrix
#and get the inverse Matrix. 

#<<- operator is used to assign a value to an object in an
#environment that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#If the value is not previously calculated the goes to the original makeCacheMatrix and does teh calculation

###We get the value of the Inverse Matrix from the makeCacheMatrix
## If return gives a NULL value calculates again inverse matrix

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  Matrixdata <- x$getMatrix()
  inverseMatrix <- solve(Matrixdata, ...)
  x$setInverse(inverseMatrix)
  return(inverseMatrix)
}
