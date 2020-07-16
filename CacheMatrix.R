## this function takes the matrix as its argument and initially sets the inverse as null
## when set() is called, it assigns the new value to x, and assigns x's inverse to NULL
## it returns a list to its parent environment
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}
##this function gets the argument and checks if inverse of matrix is already assigned
## if inverse is present, it simply returns the value
## else it solves and assigns the inverse through setinv() function and returns inverse.
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


 