## Thus function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	# clear the cache
	m <- NULL
	# define a setter for the matrix
	set <- function(y){
		x <- y
		m <<- NULL
	}
	# define a getter for the matrix
	get <- function() x
	# define setter for the matrix inverse
	setinverse <- function(inverse) m <<- inverse
	# define getter for the matrix inverse
	getinverse <- function() m
	# return methods we just define as a list
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## This function calculates the inverse of special "matrix" with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
		# try get matrix inverse
        m <- x$getinverse()
        # if exists, return ig
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # otherwise process it
        data <- x$get()
        m <- solve(data, ...)
        # store it
        x$setinverse(m)
        # return the inverse
        m
}