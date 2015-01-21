## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## You may use the test code from our discussion forum
###### Test code below
# x <- matrix(c(4,2,7,6), nrow=2, ncol=2)
# y <- makeCacheMatrix(x)
# z <- cacheSolve(y, x)
# print(z)
# z <- cacheSolve(y, x)
# print(z)
# m = x %*% z
# print(m)
# y$set(matrix(c(3,2,7,6), nrow=2, ncol=2))
# z <- cacheSolve(y, x)
# print(z)
# m = y$get() %*% z
# print(m)
###### Test code ends

makeCacheMatrix <- function(x = matrix()) {
  ## initializing the matrix
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  #the matrix x will have access to these two below functions to get and set the reverse
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  ##
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the matrix created in the last function 
  m <- x$getmatrix()
  ## Check to see whether the matrix has been created
  ## If the Matrix has been created, then send user a message
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  ## otherwise...
  ## Get the x from the anoynmous function (get), 
  ## this will look for the x value in the makeCacheMatrix function according to lexical scoping rule
  data <- x$get()
  ## Create the cached inverse using the solve function
  m <- solve(data, ...)
  ## Set the matrix 
  x$setmatrix(m)
  ## print the inverse matrix
  m
}


