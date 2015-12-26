## The functions "makeCacheMatrix" and "cacheSolve" are written on the same pattern as 
## the given functions for 'caching mean of a vector' as an example on the course page.

## "makeCacheMatrix" function creates matrix object that can cache its inverse whereas 
## "cacheSolve" function returns the inverse of the matrix returned from makeCacheMatrix and if
## it has already been calculated then then the cachesolve should retrieve the inverse from 
## the cache.
## ------------------------------------------------------------------------------------------

## "makeCacheMatrix" function contains 4 functions: set, get, setInv, getInv:
## get is a function that returns the matrix x stored in the main function.
## set is a function that changes the matrix stored in the main function.
## setInv and getInv are functions very similar to set and get. They don’t calculate the 
## inverse but simply store the value of the input in a variable m.

makeCacheMatrix <- function(x = matrix()) {
    
    xinv <- NULL 
    set <- function(y) {
        x <<- y
        xinv <<- NULL 
    }
    get <- function() x 
    setInv <- function(inv) xinv <<- inv 
    getInv <- function() xinv 
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## ------------------------------------------------------------------------------------------

## Function "cacheSolve" computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (the matrix has not
## changed), then cacheSolve should retrieve the inverse from the cache but if the inverse
## has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the 
## inverse and x$setInv(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) { 
        message("getting cached data")
        return(m) 
    }
    data <- x$get() 
    m <- solve(data) 
    x$setInv(m) 
    m 
}
