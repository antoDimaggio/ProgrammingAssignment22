## Put comments here that give an overall description of what your
## functions do

#Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # this will hold the cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # invalidate the cache when the matrix changes
  }
  
  get <- function() x  # returns the matrix
  setinverse <- function(inverse) inv <<- inverse  # stores the inverse
  getinverse <- function() inv  # retrieves the cached inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Compute or retrieve the inverses
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # check if inverse already cached
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()         # get the actual matrix
  inv <- solve(mat, ...) # compute the inverse
  x$setinverse(inv)      # cache it
  inv
}

