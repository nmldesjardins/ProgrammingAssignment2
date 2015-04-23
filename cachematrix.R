## Programming Assignment 2
## Cache the inverse of a matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {   # set value of matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x  # get matrix
        setsolve <- function(solve) m <<- solve # set inverse
        getsolve <- function() m # get value of mean
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Computes the inverse of the special "matrix" object from makeCacheMatrix,
## or retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getsolve()  # sets m to the matrix from the previous func
        if(!is.null(m)) {  # if the matrix is already computed, gets it
                message("getting cached data")
                return(m)
        } 
        data <- x$get()
        m <- solve(data, ...)  # otherwise, computes it
        x$setsolve(m)
        m
}
