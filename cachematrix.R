## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Per the assignment instructions: this function creates
## a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    # this sets the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
	

	# gets the matrix
	get <- function() {
		x
	}
	
	# sets inv, called by cacheSolve method
	setinv <- function(i) {
		inv <<- i
	}
	
	# gets inv
	getinv <- function() {
		inv
	}
	
	# return the "special" matrix
	list( set = set,
		  get = get,
		  setinv = setinv,
		  getinv = getinv
		 )    
}


## Write a short comment describing this function
## Per the assignment description: this function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix is unchanged),  
## it  retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## first we get the cached inversion
        inv <- x$getinv()
        
        ## check for cached value and return it if it is cached
        if(!is.null(inv)) {
        	message("get the cached inv")
        	return(inv)
    	}
    	
    	## if its not cached then we compute it and cache it
    	message("solving the inverse")
    	matr <- x$get()	
    	inv <- solve(matr, ...)
    	x$setinv(inv)
    	
		## and then we return it
   	 	return(inv)
}
