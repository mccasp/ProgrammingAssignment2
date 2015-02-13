## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly

## This function will check is an inverse matrix has been already calculated,
## if yes it will return it otherwise it will store it and return next time
## is required.

## In order to use this you should use like this sample:
##
## >m <- matrix(c(1:4), nrow=2, ncol=2, byrow=TRUE)
## >cm <- makeCacheMatrix(m)
## >cacheSolve(cm)
##
## You would need to call it again to see the cached data returned.
## You may have to look at this:
##
## https://class.coursera.org/rprog-011/forum/thread?thread_id=105#comment-379
##
## To have additional details

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    # this will be used to change the current matrix when you already have the 
    # instance
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinvmv <- function(imx) m <<- imx
    
    getinvmx <- function() m
    
    list(set = set, 
         get = get,
         setinvmv = setinvmv,
         getinvmx = getinvmx)
}


## This function will return matrix inverse (either computing it or using)

cacheSolve <- function(x, ...) {
    
    m <- x$getinvmx()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setinvmv(m)
    
    m
}
