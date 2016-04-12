## The followinf functions are used to cache the inverse of a matrix


## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If that's the case, it gets the result skipping the
# computation. Otherwise, it computes the inverse and sets the value in the cache

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    inv <- solve(data)
    x$setsolve(inv)
    inv
}


        

