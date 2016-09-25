## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object, which is really a list

makeCacheMatrix <- function(x = matrix()) {        
##Define the cache        
xcache <- NULL 

        set <- function(y) {
                x <<- y
        xcache <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) xcache <<- inverse

	 ##Return the xcache (inversed)
        getinverse <- function() xcache 

	 ##Return the list
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        xcache <- x$getinverse()

        	if(!is.null(xcache)) {
                message("Getting cache...")
                return(xcache)
        	}

        dat <- x$get()
        xcache <- solve(dat, ...)
        x$setinverse(xcache)
	##Return the inverse
        xcache
}
