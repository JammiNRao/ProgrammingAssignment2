## makeCacheMatrix creates a special matrix object that can store (cache) its inverse
## The inverse of a square matrix is A is the matrix InvA such that A * InvA = I (the identity matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function() i <<- solve
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the solution to matrix supplied to makeCacheMatrix 
## if it has changed, otherwise
## it returns the solutioin matrix already calculated and stored
## in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
