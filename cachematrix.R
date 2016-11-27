## These functions cache the inverse of a matrix so that when we need it again, 
## it can be looked up in the cache rather than recomputed
#
## makeCacheMatrix function creates a list containing a function to
##    1) set a matrix
##    2) get a matrix
##    3) set the inverse of a matrix
##    4) get the inverse of a matrix
#
makeCacheMatrix <- function(x = matrix()) {
        invV <- NULL 
        set <- function(y) {
                # this function sets the vector x to a new vector y, global assignment
                x <<- y
                # then resets invV to NULL, global assignment
                invV <<- NULL
         }
         # return vector x
         get <- function() x 
         # store the value of the input in variable invV, global assignment
         setinverse <- function(inverse) invV <<- inverse
         # return vector invV
         getinverse <- function() invV
         # return the special list vector containing all of the special functions
         list(set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
}
#
## cacheSolve checks to see if the inverse of a matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix data and sets the inverse 
## matrix in the cache via the setinverse function.
#
cacheSolve <- function(x, ...) {
        invV <- x$getinverse()
        # if found matrix in cache, use the cached matrix
        if(!is.null(invV)) {
                message("getting cached data")
                # return inverse of matrix from cached data
                return(invV)
        }
        # otherwise, get the matrix stored with makeCacheMatrix
        matr.data <- x$get()
        # solve will calculate the inverse of the matrix
        invV=solve(matr.data, ...)
        # store the value of the inverse in the cache via the setinverse function in makeCacheMatrix
        x$setinverse(invV)
        # return inverse of matrix from calculation
        return(invV)
}