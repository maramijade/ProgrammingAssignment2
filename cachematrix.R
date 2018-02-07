## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # set the matrix holder
  curMatrix <- NULL  # starts empty
  # get the value of the matrix
  place <- function(y)
  {
    x <<- y  # this sets the variable outside this context to be able to come back in later
    curMatrix <<- NULL #initiates another holder that we want to be empty if we haven't created an inverse
    
  }
  get <- function() x
  # set the inverse holder
  setinvhold <- function(inverse) curMatrix <<- inverse #allows for inverse to go outside this context
  # get the value of the inverse
  getinverse <- function() curMatrix
  list (place= place,get = get,
        setinvhold = setinvhold,
        getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## get the environment inverse
  curMatrix <- x$getinverse()
  ## check to see if it is null, if it is get the cached matrix
  if(!is.null(curMatrix))
  {
    message( "getting cached data")
    return(curMatrix)
  }
  
  ## preform inverse
  output <- x$get()
  curMatrix <- solve(output, ...)
  x$setinvhold(curMatrix)
  curMatrix
}
