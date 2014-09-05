
# This function creates the matrix object for caching. It contains methods for setting and getting the matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()){
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function returns the inverse of the matrix object, calculating and setting the inverse if necessary. 
cacheSolve <- function(x, ...) {

	inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting chached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}


# My suggested solution: Does not require a second function or setInverse method. The caching is included in the getInverse method of the matrix object. Works with additional options supplied to the getInverse method. If the additional options do not match the ones used for the cached calculation, the inverse is recalculated.
makeAutoCachedMatrix <- function(x = matrix()){
	inv <- NULL
	args <- NULL
	
	set <- function(y){
		x <<- y
		inv <<- NULL
		args <<- NULL
	}
	get <- function() x
	getInverse <- function(...){
		if(is.null(inv) || !identical(args, list(...))){
			inv <<- solve(x, ...)
			args <<- list(...)
		} else {
			message("getting chached inverse")
		}
		inv
	}
	list(set = set, get = get, getInverse = getInverse)
}