## Caching the Inverse of a Matrix
## Assignment2 in Coursera "R Programming" class


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(im) invMatrix <<- im
    getInvMatrix <- function() invMatrix
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getInvMatrix()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInvMatrix(invMatrix)
    invMatrix
}
