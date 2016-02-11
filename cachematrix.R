
## This function defines 4 functions: set, get, setinv, getinv
# set funcion reset input matrix if needed, and reset m to NULL if matrix is updated.
# get funcion grab the updated matrix
# setinv function gets inverse value and assign to m
# getinv grab updated m value

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


## This function first check if m exists, if not then calculate inverse then assign value to m in makeCacheMatrix environment.
## If m exists in makeCacheMatrix environment already, then print it's value without re-calculate and print a message.

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
