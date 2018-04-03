## These set of functions will help in caching Inverse of square Invertible matrix


## create a list for a given matrix which can be used to save Inverse already cached

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) m <<- inverse
   getInverse <- function() m
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Get the Inverse of the matrix using the list generated using makeCacheMatrix

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getInverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setInverse(m)
   m
}
