## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      ##this sets the value of the vector
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      ##this gets the value of the vector
      get <-function() x
      
      ##this sets the value of the inverse
      setinverse <- function(inverse) i <<-inverse
      
      ##this gets the balue of the inverse
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        }


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        ##checks to see if inverse has previously been calculated, returns if
        ## already set
        if(!is.null(i)) {
              message("getting cached data")
          return(i)
        }
        # gets inverse
        data <- x$get()
        i <- solve(data, ...)
        ## sets inverse
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
