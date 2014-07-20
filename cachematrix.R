# These are  a pair of functions that cache the inverse of a matrix

# Assumption: the matrix supplied is always invertible

# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m.inv <- NULL
    set <- function(y) {
        m <<- y
        m.inv <<- NULL
    }
    get <- function() m
    setInverse <- function(solve) m.inv <<- solve
    getInverse <- function() m.inv
    list(set = set, get = get, 
      setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m.inv <- x$getInverse()
    if(!is.null(m.inv)) {
        message("getting cached data")
        return(m.inv)
    }
    data <- x$get()
    m.inv <- solve(data, ...)
    x$setInverse(m.inv)
    m.inv
}
