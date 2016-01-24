## Author: J Weaver
##
## makeCacheMatrix is the primary function that will cache
## a matrix vector. I will return 4 nested functions = set, get, setimat, getimat
## get = return the matrix from cache
## set = assign the matrix into cache
## getimat = return the inversed matrix from cache
## setimat = assign the inversed matrix into cache

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL;
    
    #assign cache, clear inversed cache
    set <- function(y) {x <<- y; invMat <<- NULL; } 
    
    #get matrix from cache
    get <- function() {x}; 
    
    #assign inversed matrix to cache
    setimat <- function(matinv) {invMat <<- matinv;} 
    
    #get inversed matrix from cached
    getimat <- function() {invMat;}
    
    #required - return a list of func names
    list("set" = set, "get" = get, "setimat" = setimat, "getimat" = getimat) 

}

## Write a short comment describing this function
## If the inverse is cached, return the cached value.

## cacheSolve is the function that will retrieve or create the cache
## matrix if it is not cached and return the cached Matrix. The solve function
## is used that will invert the matrix data

cacheSolve <- function(x, ...) {

    #get invert matrix from cache
    invMat <- x$getimat(); 
    if(!is.null(invMat)){
        message("retrieving cached data...");
        return(invMat);
    }
    else
    {
        message("not into cache...adding to cache");
        cacheData <- x$get(); #retrieve cached matrix
        invMat <- solve(cacheData, ...);

        x$setimat(invMat); #cache inversed matrix
        return(invMat); #now return cached inversed matrix
    }
}

## How to execute

# #init the matrix and functions from makeCacheMatrix
# xMatrix <- makeCacheMatrix(matrix(runif(100,0,5), 10, 10)) 
# cacheSolve(xMatrix) # get the inverse data from cache
# xMatrix$get() # get the matrix data from cache
# 
# xMatrix$set(matrix(runif(10000,0,1), 100, 100)) # create larger matrix using set
# #repeat retrieval
# cacheSolve(xMatrix); # get the inverse data from cache
# xMatrix$get() # get the matrix data from cache
