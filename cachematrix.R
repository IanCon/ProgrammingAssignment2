## Function wrapper to handling caching and reading of matrices

## Creates the functions for creating and accessing cache objects

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL	# m for matrix
  	
	set <- function(y) {
    		x <<- y
    		m <<- NULL
  		}

  	get <- function() x
  
	setcache <- function(cache) m <<- cache		# load matrix into cache
  	
	getcache <- function() m			# read matrix in cache
  	
	list(set = set, get = get, setcache = setcache, getcache = getcache)

}


## Gets inverse matrix from cache if already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getcache()	# see if matrix required (passed as parameter x)
  				# … is in cache
	if(!is.null(m)) {
#    		message(“Cached matrix”)
    		return(m)	# found matrix in cache and return
  	}

	data <- x$get()		# get the data from the new matrix
  
	m <- solve(data, ...)	# create the inverse with ‘solve’ command

	x$setcache(m)		# and save the computed inverse matrix
  	
	m			# return the memory location

}
