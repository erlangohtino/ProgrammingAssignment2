## These functions implement matrix inversion using memory caching (in case
## the matrix inverse has already calculated, the computed result is retrieved
## instead of calculating it again.

## For this assignment, it is assumed that the matrix
## supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    ## Checking if Return a matrix that is the inverse of 'x'
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    n<-nrow(data)
    ## Build identity matrix
    I <- matrix(nrow=n,ncol=n)
    
    for (i in 1:n){
        for (j in 1:n){
            if (i==j) I[i,j]<-1 else I[i,j]<-0
        }
    }
    ## calling routine to find inverse
    inv <- solve(data, I)

    ## storing result in cache
    x$setinv(inv)
    
    ## returning inverse (when not cached)
    inv
    
}
