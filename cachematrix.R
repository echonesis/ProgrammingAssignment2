## For cachematrix.R, two functions are created for users to do the inverse matrix based on caching.
## You should try to declare a makeCacheMatrix-type list for cacheSolve input, and the object of
## makeCacheMatrix would help you save all the result, including the one returned by cacheSolve.

## makeCacheMatrix USAGE
## makeCacheMatrix    Function for making a list for saving cache matrix.
## 

makeCacheMatrix <- function(x = matrix()) {
        ## Data Member Initialization
        cm <- NULL
        
        ## Member Functions
        set <- function(y) {
                x <<- y
                cm <<- NULL
        }
        get <- function() x
        
        ## set cache matrix
        setcmatrix <- function(cmatrix) cm <<- cmatrix
        
        ## get cache matrix
        getcmatrix <- function() cm
        
        list(set = set, get = get,
             setcmatrix = setcmatrix,
             getcmatrix = getcmatrix)
}


## cacheSolve USAGE
## cacheSolve    Function for calculating the inverse matrix of 'x'.

cacheSolve <- function(x, ...) {
        ## Version 1: The input x must be the list result made by makeCacheMatrix function.
        
        ## Input Parameter Settings
        cm <- x$getcmatrix()
        
        ## Check if the inverse matrix has been solved
        if(!is.null(cm)){
                message("Try to get cached matrix for inverse caching...")
                return(cm)
        }
        
        ## If not, then try this section.
        data <- x$get()
        cm <- solve(data, ...)
        x$setcmatrix(cm)
        cm
        
        ## Another Version
        ## If type of x is matrix(), you can use these:
        ## TmpObj <- makeCacheMatrix(x)
        ## cm <- TmpObj$getcmatrix()
        ## if(!is.null(cm)){
        ##      message("Try to get cached matrix for inverse caching...")
        ## }
        ## data <- TmpObj$get()
        ## cm <- solve(data, ...)
        ## TmpObj$setcmatrix(cm)
        ## cm
}
