## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  setmat <- function(y){
    x <<- y
    invmat <<- NULL
  }
  getmat <- function() x
  setinv <- function(solve) invmat <<- solve
  getinv <- function() invmat
  list(setmat = setmat, getmat = getmat, 
       setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  if(!is.null(invmat)){
    message("getting cached data")
    return(invmat)
  }
  matdata <- x$getmat()
  invmat <- solve(matdata, ...)
  x$setinv(invmat)
  invmat
}
