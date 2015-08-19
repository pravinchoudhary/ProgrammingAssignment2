## this file, cachematrix.R, conains two functins
## 1. makeCacheMatrix: gets and sets matrix as well as inverse of the that
## 2. cacheSolve: calculates inverve of a matrix if it is not present in cache
##                else, returns from cache
##
## to call / test it for a matrix, m, use "cacheSolve(makeCacheMatrix(m))"

## function, makeCacheMatrix, creates a list of functions
## 1. set the value of matrix, x
## 2. get the value of matrix, x
## 3. set the value of inverse of matrix, x
## 4. get the value of inverse of matrix, x

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse
    invX <- NULL
    
    ## set value of matrix, x
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    
    ## bet value of matrix, x
    get <- function() x
    
    ## set value of inverse of matrix, x
    setInverse <- function(inverseX) invX <<- inverseX
    
    ## bgt value of inverse of matrix, x
    getInverse <- function() invX
    
    ## return list of all functions (getter and setters)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## function, cacheSolve,  checks of inverse of matrix, x, exists
##     if yes, then return it
##     else, calculate and set it for further user

cacheSolve <- function(x, ...) {
    ## Check if inverse value is exists in caches, if yes return
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Value is not found cache, calculate, store & return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
