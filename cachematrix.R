## This two functions "makeCacheMatrix" and "cacheSolve"  
## can be used to cache the inverse of a squared matrix

## makeCacheMatrix creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
        
}


## cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
        
}
