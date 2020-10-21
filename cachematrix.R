##The function makeCacheMatrix defines a matrix and caches 
##its inverse.  The second function, cacheSolve, either returns
##a cached inverse or calculates it and then reports the value.

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #this first part sets the value of the matrix
        #the inverse is NULL in two different environments to clear the cache
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #now defining function to set value of the matrix
        get <- function() x
        #now setting the inverse
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function calculates the inverse of the matrix from above
#and then checkes to see whether the inverse has already been 
#calculated.  If so, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() #gets inverse from cache
        if(!is.null(inv)) {
                message("getting your inverse from within cyberspace")
                return(inv)
        }
        
        #but if there's nothing in the cache, we have to calculate
        #the inverse, put it in the cache, and get the value.
        matrixvalue <- x$get()
        inv <- solve(matrixvalue) #take the inverse
        x$setinverse(inv)
        inv
}
