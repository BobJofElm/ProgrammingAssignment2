## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a List that should be saved as function of a valid square,
## invertible matrix.  This list is created so that its companion function cacheSolve
## can be executed multiple times.  During first cacheSolve execution for the saved list, the
## solve function is executed and its results saved into the inverse matrix minv in the 
## environment.  In subsequent executions against that list, the previously saved results are
## simply returned.

## use following to create a valid... testmatrix<-matrix(c(1,2,4,0,3,1,4,4,0),nrow=3,ncol=3)

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) minv <<- solve
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## Write a short comment describing this function

## cacheSolve processes a list created by its companion function makeCacheMatrix
## from an invertible square matrix.  When cacheSolve is first executed against the list,
## the inverted matrix is computed through Solve and then saved for subsequent executions
## into environment's minv matrix variable.  In subsequent executions, minv is simply returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    ##message("getting cached inverse")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  ##message("saving cached inverse")
  x$setinv(minv)
  minv
  
}
