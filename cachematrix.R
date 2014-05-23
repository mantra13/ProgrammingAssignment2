## These functions cache the inverse of a matrix

##  function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    
    #set the matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    #get the matrix
    get <- function() x
    
    #set the inverse of matrix to cache
    setInverse <- function(mInv) invMatrix <<- mInv
    
    #get the inverse of matrix from cache
    getInverse <- function() invMatrix
    
    #return list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    # check if inverse of x already stored in cache
    mInv <- x$getInverse()    
    
    if(!is.null(mInv)) {
        message("Cached inverse of matrix")
        return(mInv)
    }
    
    matx <- x$get()
    
    #inverse the matrix
    mInv <- solve(matx, ...)
    
    #store inverse to cache
    x$setInverse(mInv)
    
    #return matrix inverse
    mInv
}
