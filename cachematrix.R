## makeCacheMatrix creates an inverted matrix and stores it in the cache 
## cahceSolve retrieves the cached inverted matrix according to the 
## makeCacheMatrix function 


## The makeCacheMatrix function firstly sets the value of the matrix according 
## to x and clears any previously stored inverted matrix in im.
## Then the setter and the getter are defined in setinv and getinv.
## Finally a list is created that allows to access these operators in later
## functions.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(solve) im <<- solve
        getinv <- function() im
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve retrives the cached inverted matrix as set by
## the makeCacheMatrix function.
## If no inverted matrix has been found in x the cacheSolve 
## function sets the inverted matrix via the setinv function

cacheSolve <- function(x, ...) {
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im
}
