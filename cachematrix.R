## The following two functions can be used to cache the inverse of a matrix. 
## To save costly computation, if the inverse of a particular matrix has been 
## calculated earlier, the second function will pick up the result from the cache.
## If not, a new calculation will run.

## This function creates a special object of matrix type that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {             # input a matrix
        i = NULL                                        # reset the inverse  
        set <- function(y) {                            # save the input matrix in this environment
                x <<- y
                i<<- NULL
        }
        
        get <- function() x                             # return the value of the original matrix
        setinverse <- function(solve) i <<- solve       # calculate the inverse first time
        getinverse <- function() i                      # get the inverse from cache if already calculated
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)                   # return all functions of this object that can be accessed
}


## This function returns the inverse of the matrix created above. 
## If the inverse has already been calculated once before, then the function retrieves the result from the cache.
## Else, it will be recalculated

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
