## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        k <- NULL  # Cache for the inverse of the matrix
        set <- function(y){
                x <<- y  # Assigns a new matrix and clears the cached inverse
                k <<- NULL
        }
        get <- function() x  # Returns the current matrix
        setInverse <- function(inverse) k <<- inverse  # Sets the cached inverse
        getInverse <- function() k  # Gets the cached inverse
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)  # Returns a list of the above functions
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), it caches that inverse for faster retrieval.
cacheSolve <- function(x, ...) {
        j <- x$getInverse()  # Attempts to retrieve the cached inverse
        if(!is.null(j)){  # If cached inverse exists, it is returned
                message("getting cached data")
                return(j)
        }
        mat <- x$get()  # Retrieves the current matrix
        j <- solve(mat,...)  # Calculates the inverse of the matrix
        x$setInverse(j)  # Caches the computed inverse
        j  # Returns the inverse
}
