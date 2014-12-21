## R Programming
## Programming Assignment 2: Lexical Scoping 
## Yifeng Wu
## Dec 21, 2014

## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. The following two functions 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # set/reset inv to NULL every time makeCacheMatrix() is called
    inv <- NULL
    # set() function to obtain/store the original matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    # get() function to return the original matrix
    get <- function() { x }
    # setinv() function to obtain/store the calculated inversed matrix
    setinv <- function(solve) { inv <<- solve }
    # getinv() function to return the inversed matrix
    getinv <- function() { inv }
    # return a list of internal 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # get the cached inversed matrix in existed, or get a NULL value
    inv <- x$getinv()
    # if the cached inversed matrix in existed, directly return it withour further computation
    # also return a message
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # obtian the original matrix
    data <- x$get()
    # calcualte the inversed matrix
    inv <- solve(data, ...)
    # return the inversed matrix
    x$setinv(inv)
    inv
}

# end of this program
