## file:        cachematrix.R
## 
## author:      TopQuirk67
## date:        05/19/2014
##
## Contains:    makeCacheMatrix, cacheSolve
##
## Purpose:     The two functions are used to store the solution to a
##              matrix inversion using lexical scoping rules.  We use the 
##              <<- super assignment operator
##              to store the inverted matrix in the parent environment,
##              allowing us to retrieve the
##              matrix inversion without recalculating if the calculation
##              has been done already.
##
##              A simple set of R instructions to test these functions is:
##              > mm<-matrix(runif(9),nrow=3,ncol=3)    ## create random matrix
##              > vv<-makeCacheMatrix(mm)               ## create cache-able matrix vv
##              > yy<-cacheSolve(vv)                    ## do initial matrix inversion
##              > yy<-cacheSolve(vv)                    ## do initial matrix inversion
##              getting cached data                     ## shows that data is cached
##              > nn<-solve(mm)                         ## do matrix inversion inline
##              > identical(nn,yy)                      ## test that inversion is correct 

## function: makeCacheMatrix
##
## description: create a cashe-able matrix object
## 
## I didn't really understand this code when I read through it 
## so I commented WAY too much.  The best thread I've seen on this is
## https://class.coursera.org/rprog-003/forum/thread?thread_id=664
## 

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        ## set element function sets x and <<- makes it available in parent environment
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        ## get element returns the matrix
        get <- function() x
        ## setinv element function saves the result of the inversion for later use
        ## (i.e. it's no longer NULL)
        setinv <- function(solve) xinv <<- solve
        ## returns the cached matrix inverse
        getinv <- function() xinv
        ## a list is returned to with elements $set, $get, $setinv, $getinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates a matrix inversion, but only if there is no cached 
## inversion in the special list we have created using makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the inversion part of input
        xinv <- x$getinv()
        ## if the inverse element of the input exists, then we have already
        ## done the matrix inversion, so return the stored information.
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        ## condition is NULL inverse element, so we must calculate, set, and return inversion
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
