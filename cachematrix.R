## the two functions in this file generates a special matrix 
## and cache its inverse matrix

## This function generate a special matrix that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {

          m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## This function calculate the inverse for square matrix
## and cache it

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
