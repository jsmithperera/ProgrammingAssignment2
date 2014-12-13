## These two functions work together to calculate the inverse of a matrix and cache the result. 

## This function creates a generic list with the matrix and its inverse

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


## This function caches the inverse of a matrix and if not available, generates one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
