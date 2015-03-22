## create a matrix then cache the inverse of the matrix with two functions: 
## makeCacheMatrix
## cacheSolve


## makeCacheMatrix 
## Create a square matrix - a list containing a function to 
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of hte inverse


makeCacheMatrix <- function(x = matrix()) {
        # check to see if matrix is square:
        if (nrow(x)!=ncol(x)) {
                print("matrix is not square - exit function")
                return()
        }
        invm <<- NULL
        set <- function(y) {
                x <<- y
                invm <- matrix(rep(NA,(nrow(x))*(ncol(x))),nrow(x),ncol(x))
        }
        get <- function() x
        setinv <- function(solve) invm <<- solve
        getinv <- function() invm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)    
}

## cacheSolve
## Calculate the inverse of the matrix created in makeCacheMatrix - 
## function first checks to see if the inverse has already been calculated
## If so, cacheSolve gets the inverse from a cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the inverse in 
## the case via   setinv   function.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
                invm <- x$getinv()
                if(!is.null(invm)) {
                        message("getting cached data")
                        return(invm)
                }
                data <- x$get()
                invm <- solve(data, ...)
                x$setinv(invm)
        invm
}