## The following functions take a matrix and create a 
##cache matrix and solve for the inverse of the matrix.

## The makeCacheMatrix() function takes a matrix as an input 
##and makes it a cache matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
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


## cacheSolve() function solves for the inverse of the matrix once 
##and saves the time needed to solve the inverse again and again.

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







