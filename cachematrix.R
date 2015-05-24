## R Programming Assignment 2
## Combination of functions to cache the inverse of a matrix when possible.

## The "makeCacheMatrix" function creates a special "matrix", which is really a list containing a function to:
## 1) set the value of the matrix (set)
## 2) get the value of the matrix (get)
## 3) set the inverse of the matrix (setinverse)
## 4) get the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The "cacheSolve" function calculates the inverse of the special "matrix" created with "makeCacheMatrix".
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i    
}
