## This is the "peer assessment - programming assignment 2" for the "R Programming" module. 
## The assignment consists of writing a "caching" piece of code, which should reduce the
## calculation time for inverting a matrix (a second call for getting the inverse should 
## just get the cached value instead of re-calculating it). The assignment follows the model 
## given (caching a mean value for a vector), by creating 2 functions - one which will 
## "carry" the matrix with its inverse around, and another which will calculate and cache 
## the inverse if not already calculated. 

## The way this is supposed to be called in other code will be: 
## M <- makeCacheMatrix(...some matrix parameter)
## I <- cacheSolve(M)
## The second call will check the "cache" (stored in M) and if not present (NULL) will
## calculate the inverse and store it (in M). 


## First function of the assignment is the creation of a matrix, together with some
## accessors (seems to follow an OOP paradigm). 
makeCacheMatrix <- function(x = matrix()) {
    ## just some simple sanity checks, make sure the matrix is a matrix, square, etc
    if (! is.matrix(x))
        stop("Sorry, the argument given to makeCacheMatrix is not a matrix.")
    d <- dim(x)
    if (d[1] != d[2])
        stop("Sorry, the matrix doesn't seem to be square.")
    
    ## declare the inverse to be stored
    inverse <- NULL
    
    ## declare the "accessors" for the matrix and inverse
    
    ## "get" just returns the object
    get <- function(){
        x
    }
    ## "set" will take a matrix as param and store it
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## "getInverse" returns the inverse
    getInverse <- function(){
        inverse
    }
    ## "setInverse" sets the respective internal inverse
    setInverse <- function(y) {
        inverse <<- y
    }
    
    # return the list of these functions (accessors)
    list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## The second function will check if the inverse of the given "matrix"
## was already calculated. If so, then just return the cached thing, if not, 
## calculate and store it in the given parameter (x). 
cacheSolve <- function(x, ...) {
    ## we (should) get a matrix created with the previous method
    inv <- x$getInverse()
    if (! is.null(inv)){
        message("The inverse already calculated, returning it...")
        return(inv)
    }
    
    ## we're here, the inverse was not calculated yet, so...
    ## get the matrix encapsulated (in x)
    m <- x$get()
    ## calculate the inverse on it
    inv <- solve(m, ...)
    ## set the inverse in the x, so it can be found next time
    x$setInverse(inv)
    ## return the inverse
    inv
}
