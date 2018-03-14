

## This is a function that creates a  matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    j <- NULL

    ## Setting the matrix
    set <- function( matrix ) {
            m <<- matrix
            j <<- NULL
    }

    ## Getting the matrix
    get <- function() {
    	m
    }

    ## Setting the inverse of the matrix
    setInverse <- function(inverse) {
        j <<- inverse
    }

    ## Getting the inverse of the matrix
    getInverse <- function() {
        ## Returning the inverse property
        j
    }

    ##Returning a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ##Returning the matrix that is the inverse of 'x'
    m <- x$getInverse()

    ##Returning the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ##Getting the matrix from our object
    data <- x$get()

    ##Calculating the inverse using matrix multiplication
    m <- solve(data) %*% data

    ##Setting the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}