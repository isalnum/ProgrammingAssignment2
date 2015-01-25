## function makeCacheMatrix: creates special matrix object that can cache its inverse
## function cacheSolve: computes the inverse of the special matrix, the inverse can be retrieved from cache

## This function creates a special matrix object that can cache its inverse
## This special object can be initialized by a parameter x which should be a matrix
## Returned object contains four functions that allow the user to access the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## init inverse value to NULL
    inv <- NULL
    ## create set function will initialize variable x with matrix
    ## and variable inv with NULL because we the inverse is not computed yet
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## create get function that returns the matrix
    get <- function() x
    ## create setinverse function that caches the inverse
    setinverse <- function(inverse) inv <<- inverse
    ## create getinverse function that retuns the inverse
    getinverse <- function() inv
    ## return the four functions created above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a special matrix object as its argument
## and returns its inverse
cacheSolve <- function(x, ...) {
    ## try to get the computed inverse
    inv <- x$getinverse()
    ## if it was computed earlier (it is not NULL) then return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## otherwise get the matrix
    data <- x$get()
    ## and compute its inverse
    inv <- solve(data, ...)
    ## save this inverse to cache
    x$setinverse(inv)
    ## and also return the inverse
    inv
}
