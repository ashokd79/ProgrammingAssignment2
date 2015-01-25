## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		setm <- function(y) {
			x <<- y
			m <<- NULL
		}
		getm <- function() x
		setminv <- function(inverse) m <<- inverse
		getminv <- function() m
		list(setm = setm, getm = getm, setminv = setminv, getminv = getminv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getminv()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		data <- x$getm()
		m <- solve(data, ...)
		x$setminv(m)
		m
}