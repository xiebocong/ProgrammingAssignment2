## creates a special "matrix" that can cache its inverse by makeCacheMatrix()
## return a list which is used as the input to cacheSolve()


## makeCacheMatrix returns a list containing functions including:
## 1, set the matrix
## 2, get the matrix
## 3, set the inverse
## 4, get the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set= set, get= get, setinverse= setinverse, getinverse= getinverse)
}


## The following function returns the inverse of the matrix x. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
