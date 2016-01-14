## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix", which is really a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## The following function calculates the inverted matrix of a special "matrix" created 
## with makeCacheMatrix. It first checks to see if the inverted matrix has already
## been calculated. If so, it gets the inverted matrix from the cache and skips the
## computation. Otherwise, it calculates the inverted matrix and caches the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinv()
        if(!is.null(i)) {
                # message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
