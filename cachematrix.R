## The file contains method to create a cached version of matrix which
## can contain its inverse as well. The other method gives a way to retrieve and cache
## the inverse of the matrix  

## Takes matrix as and input and creates a list of functions to cache 
## and retrieve the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setinverse <- function(invmatrix) m <<- invmatrix
		getinverse <- function() m
		list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The function takes list returned by above function as input and returns the 
## inverse of the contained matrix from cache. If the inverse is not present in cache, it 
## creates the inverse  caches it and returns the same
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

