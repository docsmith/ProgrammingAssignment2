## Christian Alexander Graf, March 21, 2015

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix makes use of the structure of Prof. Peng's example function makeVector.
## It contains
## - a setMatrix function to redefine an existing matrix,
## - a getMatrix function to return its values
## - a getInverse to return the cached inverse of the matrix
## - a setInverse to store a matrix-value as inverse for the matrix in the cache

## The functions created are stored in a list using the corresponding names

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    
    getMatrix <- function() x
    setMatrix <- function(m){
        x <<- m
        inverted <<- NULL # since the matrix is redefined, the old inverse is no longer valid
    }
    
    getInverse <- function() inverted
    setInverse <- function(i) inverted <<- i  # Set the inverse of x to i and store it in the given instance of makeCacheMatrix
    
    list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse)
    
}


## cacheSolve takes a list-type as created by makeCacheMatrix
## If no inverse has been calculated or the matrix has not been changed, the inverse will be calculated using th R function solve
## The result is then stored using setInverse from the passed instance of makeCacheMatrix
##
## Attention: The function assumes that the matrix passed to it is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## ... contain parameter-settings for R-built-in solve function. See ?solve for more information
        
        i <- x$getInverse()
            ## set i to the currently stored inverse of matrix instance x
        
        ## if there is a cached entry for the inverse we return it
        if (!is.null(i)){
            message("retrieving inverse from cache")
            return(i)
        }
        
        m <- x$getMatrix()  # get the matrix for which we want to calculate the inverse
        i <- solve(m,...)   # calculate the inverse
        x$setInverse(i)     # store it
        i                   # return it from the function
}
