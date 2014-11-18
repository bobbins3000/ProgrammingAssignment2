## The two functions below calculate the inverse of a matrix.
## If it is the first time the matrix inverse is calculated the cacheSolve function
## will return the value (passed through by makeCacheMatrix), otherwise the value is
## cached and returned without further calculations being required

## makeCacheMatrix creates an object of type matrix with 'matrixInverse' set to NULL
## It also sets up functions get, setInverse & getInverse to be called by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) matrixInverse <<- solve
    getinverse <- function() matrixInverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve picks up 'x' from makeCacheMatrix. If there exists a cahced inverse
## it is returned, otherwise the inverse is created usign the 'solve' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getinverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setinverse(matrixInverse)
    matrixInverse
}
