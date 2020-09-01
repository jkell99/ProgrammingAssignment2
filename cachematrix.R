## This pair of functions cache the inverse of a matrix as an alternative
## to computing it repeatedly.

## This function creates a special "matrix" object that can cache its 
## inverse. The makeCacheMatrix takes an argument such as 'mymatrix' when
## mymatrix <- matrix(c(4, 7, 2, 6), 2, 2).

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache. 

## The argument for makeCacheMatrix must be the special "matrix" returned
## by makeCacheMatrix. For example, use the argument 'new_mymatrix when
## new_mymatrix <- makeCacheMatrix(mymatrix).

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        
        ## Return a matrix that is the inverse of 'x'
        i
}