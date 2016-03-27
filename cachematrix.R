## Caching the Inverse of a Matrix- these two time-saving functions allow computation
## of the inverse of a matrix opposed to computing it repetitively. 

## This function creates a special "matrix" object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL

	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by
## the makeCacheMatrix(). The function will retrieve the inverse directly from
## the cache if the inverse of the matrix has already been calculated and 
## the matrix did not change.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}
