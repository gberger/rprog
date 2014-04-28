## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # i will store the inverse
    inv <- NULL

    # set should be used to alter the matrix
    # it invalidates the cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # get simply returns the raw matrix
    get <- function() {
        x
    }

    # setinv sets the inv variable
    # should be used only by cacheSolve
    setinv <- function(i) {
        inv <<- i
    }

    # getinv gets the cached inverse
    getinv <- function() {
        inv
    }

    # return the special matrix
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)    
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # get the cached inverse
    inv <- x$getinv()

    if(!is.null(inv)) {
        # if the inverse if actually cached, just return it
        message("getting cached inverse")
        return(inv)
    }

    # otherwise, calculate the inverse and cache it
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setinv(inv)

    return(inv)
}


## Example:
# matr <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(matr)
# cacheSolve(matr)  ## "getting cached inverse"
# matr$set(matrix(5:8, 2, 2))
# cacheSolve(matr)
# cacheSolve(matr)  ## "getting cached inverse"
