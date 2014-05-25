## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL							##Clears the m variable inside the environment
  set <- function(y) {				##Defines the matrix within the environment
    x <<- y					
    m <<- NULL				
  }
  get <- function() x				##Gives the matrix defined inside the environment
  setinverse <- function(inverse) m <<- inverse	##Defines inverse of the matrix to the environment
  getinverse <- function() m					##Gives the m variable
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	  m <- x$getinverse()			##Defines the m variable as the value of the inverse already set in the environment
  if(!is.null(m)) {					##Check whether the variable m is not NULL, in other words, if there are any matrix inverse already set in the environment
  	message("getting cached data")	## In case is not NULL, print a message and return the variable of m
    return(m)
  }
  data <- x$get()					##Take the matrix content and defined as 'data'
  m <- solve(data, ...)				##Solve the inverse of 'data'
  x$setinverse(m)					##Defines the matrix inverse in the environment
  m									## Return a matrix that is the inverse of 'x'
}
