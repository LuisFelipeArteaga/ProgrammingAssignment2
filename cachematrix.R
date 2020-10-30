## Put comments here that give an overall description of what your
## functions do

## This function allows me to store the square matrix
## Input : Invertible matrix(nxn)
## Output : List 

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        set <- function(y){
                x         <<- y
                invMatrix <<- NULL
        }
        get    <- function() x
        setinv <- function(solve) invMatrix <<- solve
        getinv <- function() invMatrix
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv )
        
}


##  Calculate the inverse of the matrix
## Input : list of makeCacheMatrix
## Output : Invertible matrix(nxn) 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getinv()
        
        if(!is.null(invMatrix)){
                message("Getting cached data ...")
                return(invMatrix)
        }
        
        data      <- x$get()
        invMatrix <- solve(data, ...)
        x$setinv(invMatrix) 
        invMatrix
}
