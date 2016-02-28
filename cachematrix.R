##############################################################################################################################
## This function takes matrix as argument
## it sets current matrix to a variable, calculates inverse of matrix passed as argument
##############################################################################################################################

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##############################################################################################################################
## Function returns inverse of matrix passed in argument of above function
## If new matrix is assigned using $set then inverse is calculated again is provided if matrix is not changed function returns
## inverse result stored in cache
##############################################################################################################################

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("loading cached data")
        return(inv)
    }
    new_data <- x$get()
    inv <- solve(new_data, ...)
    x$setinv(inv)
    inv
}
