## makeCacheMatrix and cacheSolve are functions that will cache the inverse of the Matix.
## Created by Ron Rick Chua
## Programming Assignment 2 for course R Programming

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {     ###test updates 
	
	mx <- NULL                              ## Setting the value of the Matrix to null
	set <- function(y) {                    ## Creating a funtion called "set"
		x <<- y
		mx <<- NULL
	}
	
	get <- function() x                     ## Subsetting the value of the Matrix to create a function "get"
	
	setinv <- function(solve) mx <<- solve  ## Setting the Inverse Matrix values
	
	getinv <- function() mx                 ## Getting the Inverse Matrix values
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cacheSolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {  		## Return a matrix that is the inverse of 'x'

	mx <- x$getinv()			## Getting the Inverse Matrix values

	if(!is.null(mx)) {			## Checking if Matrix if it got Data or not
	message("Got data")                     ## If there is a data, print message Got Data 
	return(mx)				## and then return the data
}
	
	imdata <- x$get()			## When not found, Subset the Inverse Matrix values
	mx <- solve(imdata, ...)                ## Using Solve() to get the Inverse
	x$setinv(mx)				## Setting the Inverse Matrix values
	mx					## Print the Inverse Matrix values
}


### End of code.  :)
