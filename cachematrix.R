## cacheSolve function accepts a special matrix object return from makeCacheMatrix function, and check whether inverse of the matrix
## provided to makeCacheMatrix function is already calculated. If yes, then return cached inverse matrix, else compute inverse of the matrix

## Accepts a Matrix object (m)
## return a list of below functions
## getInvMatrix and setInvMatrix to an input Matrix object
## getInverseMatrix and setInverseMatrix of input Matrix Object using Solve function

makeCacheMatrix <- function(m = matrix()) {
invMatrix <- NULL
  setInvMatrix <- function(y) {
    m <<- y
    invMatrix <<- NULL
  }
  getInvMatrix <- function() m
  setInverseMatrix <- function(solve) invMatrix <<- solve
  getInverseMatrix <- function() invMatrix
  list(getInvMatrix =getInvMatrix, setInvMatrix = setInvMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Accepts a special object retuned by makeCacheMatrix function
## Return a matrix that is the inverse of a matrix object,input to makeCacheMatrix function
cacheSolve <- function(matrixObj, ...) {
          invMatrix<- matrixObj$getInverseMatrix()
  if(!is.null(invMatrix)) {
    ## getting inverse of the matrix using Cache
    return(invMatrix)
  }
  ## computing inverse of the input matrix for the first time
  data <- matrixObj$getInvMatrix()
  invMatrix <- solve(data, ...)
  matrixObj$setInverseMatrix(invMatrix)
  invMatrix
}
