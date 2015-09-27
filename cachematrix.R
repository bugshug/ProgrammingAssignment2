 ##Two functions to calculate the inverse of a matrix.  
## makeCacheMatrix is used to assign and retrieve the values once the inverse is calculated
## Creates a vector of functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## creates the set function which applies the input value y to the value x and nulls the cached value.
 set <- function(y) {
                x <<- y
                m <<- NULL
        }
  ## creates the get function which returns the value of x
        get <- function() x
        
  ## creates the setinverse function which applies the inverse to the cached value
        setinverse <- function(inverse) m <<- inverse
  ## gets the cached value m
        getinverse <- function() m
  ## outputs a list of the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachesolve references makecachematrix in order to check the cached variable for values and otherwise invert the 
## matrix and save the resultant value.   Takes as input the result of makecachematrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## calls the cached value of the inverse
        m <- x$getinverse()
        ## checks if the cached value and if so returns it without any calculation
        if(!is.null(m)) {
	  message("getting cached data")
	  return(m)
        }
        ## otherwise retrieves the data from makecachematrix
        data <- x$get()
        ## inverts the matrix
        m <- solve(data)
        ## and saves the result to cache
        x$setinverse(m)
        m
       
}

