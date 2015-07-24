## Put comments here that give an overall description of what your
## functions do

# makeCache Matrix initialize a "special" matrix from a standard matrix
# cacheSolve computes the inverse of the special matrix or retrieve the cached
# inverse, if available

## Write a short comment describing this function:
# This function creates the special matrix which allows to store the inverse 
# of the x matrix, it defines the functions get, set getinv and setinv used to
# manipulate the matrix. Set allows to initialize the matrix, get to retrieve 
# its value, getinv to get the inverse, if defined or NULL otherwise and setinv
# to set the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
# This function takes the "special" matrix and check whether the inverse has already been
# computed, if yes it returns it if no it computes, stores and returns it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
