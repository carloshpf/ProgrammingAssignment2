## The function makeCacheMatrix() stores the matrix to be calculated and functions
##  that will be used in the funcion cacheSolve().
## The function cacheSolve() will calculate the inverse of the matrix only if it
##  has not been calculated already.
## The following comments explain it in more details.

## This function stores three other functions: get(), setinverse() and getinverse().
## The function get() returns the value stored in x in the main function.
## The function setinverse set the value of the input (inverse) in a variable i into the
##  main function and getinverse returns it

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function firts gets the value of i in x with the function getinverse(), if
##  it is NULL it means that the inverse has not been calculated yet, otherwise it
##  just returns the value stored in i
## If the inverse has not been calculated, the function gets the matrix, solves the 
##  inverse and sets the results into i and returns it.
## Now that i has been set, it does not need to be calculated again 
##  (i will not be NULL this time).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
