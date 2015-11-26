## The two functions support the effort and inveserse a matrix and cache the result 
## to save future computation effort.


## Create matrix x with get/set methods

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


## Solve inverse matrix and cache it

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
