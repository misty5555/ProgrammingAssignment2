## write a pair of functions that  
## first creates a special "matrix" object that can
## cache its inverse
## second creates a function that computes the inverse of 
## the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculaed (and the matrix has not changed)
##then the cachesolve will retrieve the inverse from the cache


## a function, makeCacheMatrix creates a "matrix" 
## which is a list containing a function to
## set the matrix
## get the matrix
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
 
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(inverse)inv <<- inverse
        getinv <- function()inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}

cacheSolve <- function(x, ...) {
        inv <-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
