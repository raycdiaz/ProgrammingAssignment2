## These functions allow for storing an inverted matrix into its
## cache so that the process does not need to be repeated if the
## matrix has not changed

## This function creates the matrix object & allows it to be cached

makeCacheMatrix <- function(x = matrix()) {
	
	stored_inv <- NULL

	set_matrix <- function (y) {
				x <<- y
				stored_inv <<- NULL
				}

	get_matrix <- function() x
	set_inv <- function(value) stored_inv <<- value
	get_inv <- function () stored_inv

	list (set_matrix = set_matrix,
		get_matrix = get_matrix, set_inv = set_inv,
		get_inv = get_inv)
}


## This function checks if inverse is already calculated and retrieves
## from its cache, otherwise it calculates the inverse & stores to
## cache

cacheSolve <- function(x, ...) {

      value <- x$get_inv()

	if (!is.null(value)) {
		message ("Accessing cached data")
		return (value)
	}

	data <- x$get_matrix()
	value <- solve(data, ...)
	x$set_inv (value)
	value

 ## Return a matrix that is the inverse of 'x'
}
