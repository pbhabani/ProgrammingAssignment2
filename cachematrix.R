
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	## set the value of the matrix	

	set <- function(x) {
		x <<- y
		i <<- NULL
	} 

	## get the value of the matrix

 	get <- function() x

	## set the value of inverse of the matrix

      setinv <- function(inv) i <<- inv

	## get the value of inverse of the matrix

	getinv <- function() i
	
	list(set=set, get=get, setinv=setinv, getinv=getinv) 
}

cacheSolve <- function(x, ...) {

	i <- x$getinv()

	if(!is.null(i)) {
		message("Cached Data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinv(i)
	i
}
