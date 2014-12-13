## These two functions work together to calculate the inverse of a 
## matrix and cache the result. 

## The first function, makeCacheMatrix  creates a list as an object 
## and stores the original matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## The second function, cacheSolve, returns the inverse of the matrix 
## from the cache. If not cache, calculates and return the inverse.

cacheSolve <- function(x, ...) {
        
	i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
