makeCacheMatrix <- function(x = matrix()) {
	i = NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

cachesolve <- function (x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	if (!is.matrix(data)) {
		stop("the input must be a matrix")
	}
	i <- solve(data, ...)
	x$setinverse(i)
	i
}