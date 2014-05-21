## These functions create a caching infrastructure for 
## inverse matrix calculations

## Use this function to make your matrix cache-able.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcache <- function(solve) m <<- solve
        getcache <- function() m
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## Use this function to compute the inverse of a matrix, and return the 
## cached value if available.

cacheSolve <- function(x, ...) {
        m <- x$getcache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setcache(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
