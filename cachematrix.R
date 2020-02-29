## Caches the inverse of a matrix


## Create a Matrix that will cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    
    list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}


# Computes the inverse of the matrix if it has not been calculated, or
# returns the cached inverse if it has already been calculated.
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        
        if(!is.null(i)) {
            message('getting cached data')
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        
        i
}

