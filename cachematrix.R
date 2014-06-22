## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and ## the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function takes a reversible matrix as its argument and returns a list with four methods of get, set, setsolve and getsolve.

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y){
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) im <<- solve
	getsolve <- function() im
	list 	(set = set, get = get, 
		setsolve = setsolve, 
		getsolve = getsolve)
}


## This function reverses the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	im <- x$getsolve()
	if(!is.null(im)){
		message("getting cached data")
		return(im)
	}
	data <- x$get()
	im <- solve(data, ...)
	x$setsolve(im)
	im
}