## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve calculates the inverse of the matrix from makeCacheMatrix
## if the inverse has already been calculated, then the cacheSolve should
## retrieve the inverse from cache

## This function created a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)

}


## This function calculates the inverse of a matrix if the inverse has not
## already been calculated and that it has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}
