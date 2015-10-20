## makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                
                m <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solve) m <<- solve
        
        getInverse <- function() m
        
        # Return the list containing the 4 functions.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## cacheSolve returns the inverse of a matrix.
cacheSolve <- function(x, ...) {
  
        m <- x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached matrix")
          
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setInverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        return(m)
}

