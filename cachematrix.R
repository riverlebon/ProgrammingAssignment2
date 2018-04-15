## These function cache and recover the inverse of a matrix
## Riv on Apr 14, 2018

## This function creates a special "matrix" object that can cache its inverse
## try with 
## mym <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## im <- makeCacheMatrix(mym)
## cacheSolve(im)

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
                x <<- y
                m <<- NULL
            }
     get <- function() x
     setInv <- function(inv) m <<- inv
     getInv <- function() m
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}


## return the inverse of the "matrix" x created by makeCacheMatrix. 
## If it's been already calculates, retrieve it from the cache

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
		message("getting cached data")
		return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
