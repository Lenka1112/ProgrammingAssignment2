## First I will create a list that will keep information about the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    seti <- function(solve) i <<- solve
    geti <- function() i
    list(set = set, get = get, seti = seti, geti = geti)
}


## In the below function I will invert the matrix or read it from cache

cacheSolve <- function(x, ...) {
    i <-x$geti()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$seti(i)
    i
}
