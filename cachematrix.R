## Cache the inverse of a matrix in order to avoid extensive computation on large matrices or repeated computation.

##Creates a list of 4 functions, (set, get, setInverse, getInverse), to operate on input matrix and its inverse
makeCacheMatrix <- function(mat = matrix()) {
        imat <- NULL
        set <- function(y) {
                mat <<- y
                imat <<- NULL
        }
        get <- function() mat
        setInverse <- function(solve) imat <<- solve
        getInverse <- function() imat
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
##Uses the makeCacheMatrix object to find the inverse of a matrix. If the matrix has already been cached, it skips the 
##computation.
cacheSolve <- function(x, ...) {
        imat <- x$getInverse()
        if(!is.null(imat)) {
                message("getting cached data")
                return(imat)
        }
        data <- x$get()
        imat <- solve(data, ...)
        x$setInverse(imat)
        imat
}