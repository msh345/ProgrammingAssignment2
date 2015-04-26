## These functions cache and retrieve the inverse of a matrix. The
## point is to store the calculation of an inverse matrix so it 
## doesn't need to be calculated again.

## This function takes a matrix as an argument and returns a list 
## of 4 functions which: 1) set the matrix, 2) get the matrix, 3)
## set the inverse of the matrix, & 4) get the inverse of the matrix.

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


## This function takes the list of 4 functions returned by makeCacheMatrix
## as an argument and calculates the inverse of the matrix, if it hasn't been
## calculated already. If it has been calculated, then the function just 
## returns the inverse of the matrix stored in memory.

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