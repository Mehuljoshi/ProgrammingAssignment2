# Below are two functions named makeCacheMatrix() and cacheSolve() that are used to create a 
# special object that stores a matrix and cache's the inverse of that matrix.

# The first function makeCacheMatrix() is passed a matrix as an input argument. 
# This function makeCacheMatrix() would then create an "object" of type "list".

# This object stores two thing: 
# a) the original matrix's value (see code: x <<- y)
# b) the cached value, which is initially set to 'NULL' (see code: m <<- NULL)

# Note here the <<- operator is used to assign a value to an object in an environment
# that is different from the current environment.

# This object being created is a generic storage object (you can store a number representing
# anything, not just the inverse of a matrix). The key point here is you use makeCacheMatrix()
# to create an object, then access that object, not makeCacheMatrix

# The function makeCacheMatrix() creates a special "matrix", which is really a list containing
# four functions to
# a) set the value of the matrix, i.e set <- function(y)

# b) get the value of the matrix, i.e get <- function() {x}, 
# this function returns the value of the original matrix

# c) set the value of the inverse of the matrix using solve, 
# i.e. setinverse <- function(solve) {m <<- solve}
# this is called by cacheSolve() during the first cacheSolve() access, and it will
# store the value using superassignment

# d) get the value of the inverse, i.e. getinverse <- function() {m}
# this will return the cached value to cacheSolve() on subsequent accesses.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setinverse <- function(solve) {m <<- solve}
  getinverse <- function() {m}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# The funciton cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix.
# cacheSolve() accesses the object (not the makeCacheMatrix() function), by fetching the value of the 
# matrix used to create the object. This matrix is being stored when the object was created.
# If the inverse has not yet been calculated (='NULL'), cacheSolve() calculates the inverse and stores
# it in the object created by the call to makeCacheMatrix(), then returns the inverse.
# If the inverse has been calculated earlier (and the matrix has not changed) then cacheSolve() simply fetches 
# it and returns the inverse value from the cache, saving the computing time required to calculate the inverse again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}