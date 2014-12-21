## Put comments here that give an overall description of what your
## functions do

## There are two key functions here.
## function makeCacheMatrix creates a special matrix with its inverse precalculated
## function cacheSolve returns the inverse of the matrix, if in cache it uses the cached value
## if not calculates the inverse and stores it using the makeCacheMatrix function

## Write a short comment describing this function
## Creating a special matrix object with its inverse precalculated
makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        set <- function(y){
                x <<- y
                mInv <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) mInv <<- solve(x)
        getSolve <- function() mInv
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)

}


## Write a short comment describing this function
## THis function returns the inverse of the matrix, if the value is precalculated
## it is retrieved from cache, else calculated and stored in the special matrix object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## mInv will be the inverse of the matrix 'x' here
        mInv <- x$getSolve()
        if(!is.null(mInv)){
                message("getting cached inverse of the matrix")
                return(mInv)
        }
        #no cached inverse, calculate the inverse
        data <- x$get()
        mInv <- solve(data)
        x$setSolve(mInv)
        mInv
}
