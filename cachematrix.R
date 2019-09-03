## Assignment: Caching the Inverse of a Matrix


## makeCacheMatrix creates a special matrix and sets its atributes accordingly

makeCacheMatrix <- function(x = matrix()) {
        cInv <- NULL                            # Initially sets the inverse variable to null
        set <- function(y) {                    # calling this function will reassign the object with new matrix and set the cached value to Null.
                x <<- y
                cInv <<- NULL
        }
        get <- function() x                     # returns present matrix.
        setInv <- function(invM) cInv <<- invM  # lexical scoping used to assign value by child function.
        getInv <- function() cInv               # returns inverse matrix after cachesolve called once.
        list(set = set, 
             get = get,
             setInv = setInv,
             getInv = getInv)                   # function returns a list.
}


## This function computes the inverse of the special 
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        invM<-x$getInv()                                # gets the value of Inverse matrix if it's already cached in passed object.
        if (!is.null(invM))                             # if value is already cached, it will return the cached value.
        {
                message("getting cached data...")
                return(invM)
        }
        mtrx<-x$get()                                   # get the matrix in passed object.
        invM<-solve(mtrx)                               # calculate the inverse of matrix.
        x$setInv(invM)                                  # set the same in given object
        invM                                            # Return a matrix that is the inverse of 'x'
}
