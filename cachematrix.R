## Put comments here that give an overall description of what your
## functions do

##The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the solve reverse
## 4. get the value of the solve reverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix)
}

## Return a matrix that is the inverse of 'x', checks if reverse of matrix is already calculated. if do the reverese vlaue of matrix is
## obtained from cache and skips the reversing process using computation. Otherwise, it calculates the reverse of the matrix and sets the value of the matrix in the cache via the setmatrix function.
cacheSolve <- function(x = matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}