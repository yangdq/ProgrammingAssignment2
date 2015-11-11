## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}

	get <- function() x
	setInverse <- function(inv) inver <<- inv
	getInverse <- function() inver
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(! is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
