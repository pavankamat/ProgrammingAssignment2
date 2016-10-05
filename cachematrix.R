## The two functions below calculate the inverse matrix and retrieve the matrix from a cache

## makeCacheMatrix function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	##get returns the matrix stored in makeCacheMatrix
        get <- function() x

	##set changes to the matrix stored in makeCacheMatrix
        setinv <- function(solve) m <<- solve

        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the “matrix” 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
		##retrieve inverse matrix from the cache
                message("getting cached data")
                return(m)
        }
	##get the matrix stored with makeCacheMatrix
        data <- x$get()

	##calculate the inverse
        m <- solve(data, ...)

	##store the inverse matrix in makeCacheMatrix
        x$setinv(m)
        m
}
