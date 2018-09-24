
## makeCacheMatrix - its a function that creates a matrix that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #
        # clears the prior cache
        set <- function(y) {
                x <- y    # set value
                m <- NULL # Clears the cache
        }
        
        get <- function(x) # set the inverse
                setinverse <- function(inverse) m <- inverse
        
        getinverse <- function(m) # set the inverse
                
                #Returns the list
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}

## cachesolve is a function that uses the inverse of the makeCacheMatrix.
## if there is no change to the matrix then cache solve will get the inverse from the cache.



cachesolve <- function(x) {
        m <- x$getinverse() #looks for cache
        if(!is.null(m)) { 
                
                message("getting cached data") # This is function works to see if the cache is empty

                return(m)
        }
        # If the cache is empty then it has to be calculated, cached, and then once cached, returned.
        data <- x$get()  # get the matrix value
        m <- solve(data) # This is the caculation inverse
        x$setInverse(m)  # this is the cache result
        m                # return the inverse
}








