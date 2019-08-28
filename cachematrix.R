## Assignment: Caching the Inverse of a Matrix


## makeCacheMatrix creates a special matrix and sets its atributes accordingly

makeCacheMatrix <- function(x = matrix()) {
        cInv <- NULL
        set <- function(y) {
                x <<- y
                cInv <<- NULL
        }
        get <- function() x
        setInv <- function(invM) cInv <<- invM
        getInv <- function() cInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special 
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        invM<-x$getInv()
        if (!is.null(invM))
        {
                cat("getting cached data...")
                return(invM)
        }
        mtrx<-x$get()
        invM<-solve(mtrx)
        x$setInv(invM)
        invM
        ## Return a matrix that is the inverse of 'x'
}
