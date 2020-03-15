## The following 2 functions cache the inverse of a matrix and they are meant to
## avoid repeated computation. 

## This function creates a list - a special "matrix" - which contains functions
## to set the value of the matrix, get its value, as well as set and get the value
## of the inverse of the matrix. The special "matrix" created by the function can
## cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y){
      x <<- y
      inv_matrix <<- NULL
  }
    get <- function()x
    set_invm <- function(invm)inv_matrix <<- invm
    get_invm <- function()inv_matrix
    list(set = set, get = get,
       set_invm = set_invm,
       get_invm = get_invm)

}


## This function calculates the inverse of the special "matrix" returned by the 
## previous function. In case the inverse has been calculated already, then it
## returns the message "getting cached data" and it gets the value from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$get_invm()
    if(!is.null(inv_matrix)) {
      message("getting cached data")
      return(inv_matrix)
  }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$set_invm(inv_matrix)
    inv_matrix
}
