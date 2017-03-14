##Matrix inversion is resource and time intesive, and caching the inverse of a matrix is easier than
##computing it continuously. The below functions cache the inverse of a matrix.

## makeCacheMatrix creates a list and sets and gets the value of the matrix and it's inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)

  # setting the value of the inverse.
  x$setinv(inv)

  return(inv)

}
