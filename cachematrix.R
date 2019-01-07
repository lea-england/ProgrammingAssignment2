

## The first function, makeVector creates a list containing a function to

##1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The function below computes the inverse of the matrix created in the function above, first checking if the inverse has already been calculated and the matrix not changed. If it has been calculated it returns the already cached inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("Getting cached data...")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
