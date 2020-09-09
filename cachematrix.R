## Put comments here that give an overall description of what your
## functions do

#makeCacheVector creates a special matrix, which is really a list containing a function to

#1. et the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Calculates the inverse of the special "matrix" created by makeCacheMatrix.
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the matrix from the cache and skips the computation. Otherwise, 
#it calculates the matriz of the data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}
