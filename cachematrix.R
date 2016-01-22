## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss 
## here). 
##
## The following two functions work together to create a square invertible 
## matrix and make the inverse of the matrix available in the cache environment.
##
## Sample run:
##
## > y <- matrix(1:4,2,2)
## > m <- makeCacheMatrix(y)
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## No cache in the first run
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Retrieving from cache (second run)
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## This makeCacheMatrix() function creates a special "matrix" object that can 
## cache its inverse. It creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse value of the matrix
## 4. get the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
        # initialize the cached value to NULL
        invMatrix <- NULL
        # creates the matrix in the working environment
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # invert the matrix and store in cache
        setInverse <- function(inverse) invMatrix <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() invMatrix
        # return created functions to the working environment
        list(set=set, 
             get=get, 
             setInverse=setInverse, 
             getInverse=getInverse)
}

## This cacheSolve() function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Attempt to get the inverse matrix stored in cache
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)){
                message("getting cached data")
                ## Return a matrix that is the inverse of 'x'
                return(invMatrix)
        }
        ## Else create the matrix in working environment since it does not exist
        data <- x$get()
        invMatrix <- solve(data)
        ## Set inverted matrix in cache
        x$setInverse(invMatrix)
        ## Return a matrix that is the inverse of 'x'
        invMatrix        
}