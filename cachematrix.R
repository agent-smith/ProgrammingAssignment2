## These two functions are used to create a special object that stores a matrix
## and caches its inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mtx = matrix()) {
    inv_mtx <- NULL
    set <- function(x) {
      mtx <<- x
      inv_mtx <<- NULL
    }
    get <- function() mtx
    setinv <- function(inv) inv_mtx <<- inv
    getinv <- function() inv_mtx
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
    inv_mtx <- mtx$getinv()
    if(!is.null(inv_mtx)) {
      message("getting cached data")
      return(inv_mtx)
    }
    data <- mtx$get()
    inv_mtx <- solve(data, ...)
    mtx$setinv(inv_mtx)
    inv_mtx
}
