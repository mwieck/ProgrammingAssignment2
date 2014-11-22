##makeCacheMatrix creates a vector which is a list containing functions to
##1. set the values of the matrix
##2. get the values of the matrix
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) { ## x is the matrix which can be accessed via the cacheSolve function to create the inverse
 
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get= get, setsolve= setsolve, getsolve = getsolve)
}

##The cacheSolve function calculates the inverse of the special vector created with the makeCacheMatrix function. 
##It first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the invers of the matrix and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) { ## x is the special vector
        
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}
