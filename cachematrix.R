
## makecacheMatrix caches values for faster retrieval 
## locality: the closer it is to the file, the quicker the retrieval
## memory hierachy tries to exploit locality
## On-chip cache can process quicker, which is where this function will be

makeCacheMatrix <- function(x = matrix()) {
	## creates an undefined matrix which will be filled below
	
	inv <- NULL
	## setting the inverse to NULL, often an expression and/or function
	## whose value is undefined
	## will throw values in there later on in the function

	set <- function(y) {
		
		x <<- y
		inv <<- NULL
		## '<<-' assigns a value to an object in an environment different from
		## the current environment
		## y becomes x now
		
	}

	get <- function() x 
	setinv <- function(inverse) inv <<-inverse
	## sets the inverse.  'inv' from above no longer null
	## inv becomes inverse from function

	getinv <- function() inv
	## retrieves the inverse

	list(set=set, get=get, setinv=setinv, getinv=getinv)
	## pulls from above
	## set = set = function(y)
	## get = get = function()x
	## getinv = getinv = function() inv

}


## cacheSolve() computes the inverse of the matrix returned by makeCacheMatrix()
## notice during the test when it says "grabbing cached data" v. a simple print
## although small for the matrices you tested with, this could make a difference on a 
## larger matrice

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	inv = x$getinv()
	
	if(!is.null(inv)) {
		message("grabbing cached data")
		return(inv)
		## if the inverse is not null from above, meaning it has already 
		## been cached, this command skips the computation and gets it 
		## from the cache. 
	}
	
	matrix_data <- x$get()
	inv <- solve(matrix_data, ...)
	##if the inverse is null, meaning it has not been cached, this will
	##calculate the inverse 

	x$setinv(inv)
	return(inv)
	## this will set the value of the invest and store it in the cache
	## as taken from immediately above. 
}
