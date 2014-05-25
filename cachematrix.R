## our function creates a new matrix from given invertible matrix , and returns a inverse while saving the same
## so that it doesnt have to calculate each time

## This function creates a matrix in cache of the given matrix by us which is invertible


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- N
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function creates the inverse of the cached matrix of thr previous function

cachesolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
