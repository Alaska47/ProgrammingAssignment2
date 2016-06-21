## this funtion creates a list that contains functions that set the matrix, get the matrix, set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    return(x)
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() {
    return(inv)
  }
  return(list(set=set, get=get, setinverse=setinverse, getinverse=getinverse))
}


## this function checks to see if the inverse of the matrix is already calculated and if so, returns it, or else it calculates the inverse then returns it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  temp_inv <- x$get()
  inv <- solve(temp_inv, ...)
  x$setinverse(inv)
  return(inv)
}
