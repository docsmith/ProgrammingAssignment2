## Christian Alexander Graf, March 21, 2015

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix makes use of the structure of Prof. Peng's example function makeVector.
## It contains
## - a setMatrix function to redefine an existing matrix,
## - a getMatrix function to return its values
## - a getInverse to return the cached inverse of the matrix
## - a setInverse to calculate and store the inverse in cache
## The functions created are stored in a list using the corresponding names
## Attention: The function assumes that the matrix used is invertible
## Please note: Contrary to the vector example the calculation of the inverse is already done when setInverse is invoked.

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    
    getMatrix <- function() x
    setMatrix <- function(m){
        x <<- m
        inverted <- NULL # since the matrix is redefined, the old inverse is no longer valid
    }
    
    getInverse <- function() inverted
    setInverse <- function() inverted <<- solve(x)  # Calculate the inverse of x and store it in inverted
    
    list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
