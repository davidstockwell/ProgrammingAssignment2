# The following two functions allow you to cache the solution of a matrix
# for the sake of optimisation

# makeCacheMatrix creates a list object containing four functions
# which get and set the source data and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve will take the return value from makeCacheMatrix and 
# return the inverse if it has been calculated, or if not, calculate
# it, cache it and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
