## makeCacheMatrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	 inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_matrix <<- inverse
        getInverse <- function() inv_matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve:
## This funcion computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated, then it retrieves the invers from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getInverse()
        if (!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        mat <- x$get()
        inv_matrix <- solve(mat, ...)
        x$setInverse(inv_matrix)
        inv_matrix
}