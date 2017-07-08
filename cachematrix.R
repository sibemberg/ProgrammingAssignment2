## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # init inv
    set <- function(m) { # set the matrix x
        x <<- m
        inv <<- NULL
    }
    get <- function() { x } # get the matrix x
    setinv <- function(i) { inv <<- i } # set the inv matrix of x
    getinv <- function() { inv } # get the inv matrix of x
    list(set = set, get = get, setinv = setinv, getinv = getinv) # listing
}


# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
# If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    im <- x$getinv() # get inv matrix of matrix x
    if(!is.null(im)) { # if it is not null, return it
        message("getting cached data")
        return(im)
    }
    # otherwise, calculate it
    data <- x$get()
    im <- solve(data, ...)
    x$setinv(im)
    im
}
