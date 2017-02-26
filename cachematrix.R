## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Two functions: Inverse Matrix and Cache Inverse Matrix
## Function 1: creates special "matrix"objects that can cache its inverse

#' @param x 
#'
#' @return
#' @export
#'
#' @examples
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  f <- function(y) {
    x <<- y
    inverse <<- NULL 
  }
  get <- function() x 
  finverse <- function(solveMatrix) inverse <<- solveMatrix
  finalinverse <- function () inverse 
  list(f = f, get = get, finverse = finverse, finalinverse = finalinverse )
}


## Function 2 computes the inverse of the "matrix" calculated in function 1

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$finalinverse()
  if(!is.null(inverse)){
    message("get cached inversed matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$finverse(inverse)
  inverse
}
