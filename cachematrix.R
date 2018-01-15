## The functions changes a matrix into a 'special' matrix that saves(caches) the inverse of it,
## and use that 'special' matrix to find the inverse without computing it repeatedly.


## makeCacheMatrix returns a 'special' Matrix that includes functions to set, get a matrix 
## and the functions to set, get the inverse of it


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, only computing the inverse if it was not previously computed. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i   
}
