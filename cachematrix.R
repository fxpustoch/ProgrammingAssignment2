# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
# a matrix rather than compute it repeatedly 
# Here is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve omputes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Test Results of the solution

# > source("~/Desktop/Coursera/ProgrammingAssignment2/cachematrix.R")
# > example_matrix <- makeCacheMatrix(matrix(1:4,2,2))
# > example_matrix$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > example_matrix$getinverse()
# NULL
# > cacheSolve(example_matrix)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > example_matrix$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > example_matrix$getinverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > A <- example_matrix$get()
# > B <- example_matrix$getinverse()
# > A %*% B
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 