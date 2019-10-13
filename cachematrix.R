
## makeCacheMatrix & cacheSolve are the functions that cache and compute the inverse of a matrix

## This function creates a special "matrix" object that can cache its inversed version

makeCacheMatrix <- function(mtx = matrix()) {
    inversedMatrix <- NULL

    get <- function() return(mtx)
    set <- function(newMtx) {
        mtx <<- newMtx
        inversedMatrix <<- NULL
    }

    getinv <- function() return(inversedMatrix)
    setinv <- function(newInversedMatrix) inversedMatrix <<- newInversedMatrix
    
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the matrix
## If inversed matrix was calculated before, it will be returned, skipping repeating calculation

cacheSolve <- function(mtx, ...) {
    inversedMatrix <- mtx$getinv()

    if(!is.null(inversedMatrix)) {
        message("Getting cached data...")
        return(inversedMatrix)
    }

    data <- mtx$get()
    inversedMatrix <- solve(data, ...)
    mtx$setinv(inversedMatrix)

    return(inversedMatrix)
}
