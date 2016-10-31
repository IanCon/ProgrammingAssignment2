## Function wrapper to handling caching and reading of matrices

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	
	set <- function(y) {
    		x <<- y
    		m <<- NULL
  		}

  	get <- function() x
  
	setcache <- function(cache) m <<- cache
  	
	getcache <- function() m
  	
	list(set = set, get = get, setcache = setcache, getcache = getcache)

}


## Gets inverse matrix from cache if already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getcache()
  
	if(!is.null(m)) {
#    		message(“Cached matrix”)
    		return(m)
  	}

	data <- x$get()
  
	m <- solve(data, ...)

	x$setcache(m)
  	
	m

}
