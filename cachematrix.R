#get matrix input
#Computing the inverse of a square matrix
#set the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(invMatrix) m <<- invMatrix
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix, # set the value of the inverted matrix
       getInvMatrix = getInvMatrix) # get the value of the inverted matrix
}


# solve function is used to invert the matrix
# sets the value of the inverted matrix using setInvMatrix function

cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) 
  x$setInvMatrix(m) 
  m
}