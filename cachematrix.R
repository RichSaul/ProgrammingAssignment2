## The functions defined in cachematrix.R work together to define a cached matrix
## object and to a calculate and cache the correlative inverse matrix.

## Sample matrices and their inverses, for testing and verification
#o     <- matrix(c(3,4,5,7), nrow=2, ncol=2, byrow=TRUE)
#o_inv <- matrix(c(7,-4,-5,3), nrow=2, ncol=2, byrow=TRUE)
#p     <- matrix(c(1,0,0,-5,1,0,20,-4,1), nrow=3, ncol=3, byrow=TRUE)
#p_inv <- matrix(c(1,0,0,5,1,0,0,4,1), nrow=3, ncol=3, byrow=TRUE)
#q     <- matrix(c(1,0,0,0,1,0,0,-4,1), nrow=3, ncol=3, byrow=TRUE)
#q_inv <- matrix(c(1,0,0,0,1,0,0,4,1), nrow=3, ncol=3, byrow=TRUE)
#r     <- matrix(c(1,0,0,0,1,1,0,0,1,2,1,0,1,3,3,1), nrow=4, ncol=4, byrow=TRUE)
#r_inv <- matrix(c(1,0,0,0,-1,1,0,0,1,-2,1,0,-1,3,-3,1), nrow=4, ncol=4, byrow=TRUE)


## makeCachedMatrix defines a cached matrix object with four functions
## makeCachedMatrix requires a matrix as a parameter -- like the sample values
## o, p, q, and r above -- and returns an object with four functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse matrix of a cached matrix object.  If the cached
## matrix already has an inverse, cacheSolve returns it; otherwise, cacheSolve 
## calculates and stores the invserse to the cached matrix object using its setinv()
## function.  cacheSolve requires a cached matrix object (output of makeCacheMatrix)
## as an input parameter and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
