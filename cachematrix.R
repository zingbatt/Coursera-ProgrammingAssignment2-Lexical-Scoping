## makeCacheMatrix and cacheSolve create a "CacheMatrix" object that stores
## the inverse of a matrix and has 4 functions allowing that inverse to be
## calculated and retrieved

## makeCacheMatrix takes a matrix and returns a list of 4 functions allowing
## the matrix and its inverse to be set and retrieved

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   #holds inverse of matrix
        setMatrix <- function(mtrx) {
                x <<- mtrx
                inv <<- NULL
        }
        getMatrix <- function() x
        setInv <- function(i) inv <<- i
        getInv <- function() inv
  
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv= getInv)
}


## cacheSolve checks whether a makeCacheMatrix object contains a stored
## inverse and if so gets & returns it, otherwise it calculates the inverse
## and stores it in the makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
