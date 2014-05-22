# These functions create a caching infrastructure for 
# inverse matrix calculations

## Use the makeCacheMatrix function to make your matrix "cache-able".  The  
## matrix 'x' and inverse martix 'm' are stored in the environment of the 
## makeCacheMatrix function, although nested functions with their own 
## environments are utilized as well.

makeCacheMatrix <- function(x = matrix()) {
### m will be the inverse matrix.  x is the matrix.
        m <- NULL
### Store the matrix in the environment of the makeCacheMatrix function.        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
### Get the matrix from the environment of the makeCacheMatrix function.
        get <- function() x
### Stores the inverse matrix in the environment of the makeCacheMatrix function.
        setcache <- function(solution) m <<- solution
### Gets the inverse matrix from the environment of the makeCacheMatrix function.
        getcache <- function() m
### Create a list of functions you can call from the parent environment in the 
### format x$set, x$get, x$setcach, and x$getcache.  
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}

## Use cacheSolve function to compute the inverse of a matrix, and return the 
## cached value if available.

cacheSolve <- function(x, ...) {
### Get the cached data if available and return the inverse matrix        
        m <- x$getcache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
### Solve the problem and cache the inverse matrix if this is the first time.        
        data <- x$get()
        m <- solve(data, ...)
        x$setcache(m)
### Return the inverse matrix.
        m
}
