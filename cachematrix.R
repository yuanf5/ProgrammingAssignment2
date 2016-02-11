## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function defines 4 functions: set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

##Test:
mx <- matrix(c(7,6,8,5), nrow=2)
z <- makeCacheMatrix(mx)
cacheSolve(z)
cacheSolve(z)  ## Getting cached data when run 2nd time
