## The following lines contain a set of two functions aimed at creating an 
## object storing a square matrix, calculating its inverse value and storing 
## its inverse value in cache.
##
## The function makeCacheMatrix() takes a sqaure matrix as its argument and
## returns a list containing the matrix, a value holder for its inverse and 
## four functions for later usage by the cacheSolve() function. The functions
## contained have the following uses:
##      1. Sets the value for the square matrix
##      2. Returns the square matrix
##      3. Setting and cache-ing the inverse value for the last matrix
## passed to the cacheSolve function
##      4. Returns the inverse value of the matrix (NULL if not yet calculated
## and cache'd)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL 
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)      
}

## The following function takes the created object by makeCacheMatrix as its
## argument and calculates the inverse value of the square matrix. It first 
## checks if the inverse value for that specific object has already been 
## calculated and, if so, skips the computation and returns it, stating it 
## is getting cached data. If the inverse value has not yet been calculated, 
## it calculates it and sets the inverse value in the cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("Getting cached data!")
                return(inv)
        } else {
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
                inv
        }
}
