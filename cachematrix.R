## These two functions combine to  calculate the inverse of a matrix.  The matrix and inverse  
##  are both cached, so that subsequent queries will retrieve the cached inverse
## instead of recalculating.

## makeCacheMatrix creates a function that caches a matrix and its inverse.  These variables
##  are stored in the global environment.  The makeCacheMatrix function returns a list with
## the name of the newly constructed function

makeCacheMatrix <- function(x = matrix()) {        
        cacheMatrix <- function(x) {
            cachedM <<- x  
            cachedInv <<- solve(x)
        }
    ## Return list of the constructed function name
    list(cacheMatrix=cacheMatrix)
}


## cacheSolve accepts an invertible matrix as its argument, and determines if x should
## be solved by calculation or if the solution may be retrieved from a cache.

cacheSolve <- function(x, ...) {
    ## These command lines determine if there is a previously cached solution, and if it
    ## is applicable to x.
    if(exists("cachedM")) {
        if(identical(x,cachedM) && !is.null(cachedInv)) {
            return(cachedInv)
        }
    }
    ## These commands execute the first function, makeCacheMatrix, to solve for x and cache 
    ## both x and its inverse
    a <- makeCacheMatrix(x)
    a$cacheMatrix(x)
    cachedInv
}
