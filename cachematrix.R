
# Function that creates a special matrix that can cache the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # Function for seting the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Function for geting the value of the matrix
        get <- function() x
        
        # Function for seting the value of the cached matrix
        setinverse <- function(inverse) inv <<- inverse
        
        # Function for geting the value of the cached matrix
        getinverse <- function() inv
                
        # Return a list of all internal functions of the CacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Function that finds and cache the inverse of a CacheMatrix
# If the inverse has been calculated before the cached answer is returned
cacheSolve <- function(x,...) {
        inv <- x$getinverse()

        # If the inverse already have been calculated return the cached result
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Otherwise calculate the inverse and store it in the cache
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)
        inv
}

