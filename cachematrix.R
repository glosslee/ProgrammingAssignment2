## GL 2015/04/20
## To solve and cache a inverse matrix

## makeCacheMatrix is to cache the existing inverse matrix in oder to
## speed up calculation

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setCacheMatrix <- function (matr) m <<- matr
        getCacheMatrix <- function () m
        
        list (set = set, get = get, setCacheMatrix = setCacheMatrix, 
              getCacheMatrix = getCacheMatrix)
}


## cacheSolve: to either solve the matrix or determine it is in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getCacheMatrix
        if (!is.null(m)) {
                message ("getting cached data")
                return m
        }
        data <- x$get()
        m <- solve(data)
        x$setCacheMatrix(m)
        m
}
