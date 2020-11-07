## The function calculates the inverse of a matrix
## puts the result in the cache
## makeCacheMatrix creates a list containing a function to set and get values

makeCacheMatrix <- function(x = matrix()) {
    invMAtrix <- NULL
    setMatrix <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix 
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of the above function. It gets or sets the mean inverse

cacheSolve <- function(x, ...) {
    invMAtrix <- x$getInverse()
    if(!is.null(invMatrix)) {
        message("Retrieving cached inv matrix")
        return(invMatrix)
    }

    MatrixData <- x$getMatrix()
    invMatrix <- solve(MatrixData, ...)
    x$setInverse(invMatrix)
    return(invMatrix)
}
