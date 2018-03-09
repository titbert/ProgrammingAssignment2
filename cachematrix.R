## This is a pair of functions. makecachematrix will create a matrix object which can cache its inverse
## cacheSolve will calculate the inverse of the matrix returned by makeCacheMatrix, or if it is already stored in cache, retrieve it from there.

## To begin, makecachematrix will:
## Take the argument, y, and assign its value (input) to x in the parent environment.
## pass NULL value to i in the parent environment.
## x and i are defined in the makeCacheMatrix environment
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

## Then with get, fetch the vector x from makeCacheMatrix environment.  
## setinverse stores value of input into the variable i
## This value is the inverse of the vector x
## getinverse fetches the inverse, i
## Then all the functions are gathered together in a special vector
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cachesolve will check to see if the value of i is not NULL, i.e. if the inverse has already been cached in i, and if it is, return it.
## If it is not cached, the vector x is fetched and the inverse is solved and stores it in the i variable in the makeCacheMatrix
cacheSolve <- function(x, ...) {
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

