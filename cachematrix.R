# Computing the inverse of a square matrix can be done with the solve function in R.
# For example, if X is a square invertible matrix, then cacheSolve(X) returns its inverse.

# Caching matrix function
# The function below creates a list containing methods to get and set values for matrix itself and for the inverted one.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Computing the inverse of matrix returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
