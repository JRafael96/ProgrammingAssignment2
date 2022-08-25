## Put comments here that give an overall description of what your
## functions do



#  makeCacheMatrix function creates a matrix 
#  which contains a list of functions:
#   
# - set the elements of the matrix
# - get the elements of the matrix
# - set the elements of the matrix inverse
# - get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

# cacheSolve function calculates the inverse of the matrix created with the previous
# function. But before doing that, checks if the matrix has been already calculated,
# if its already done takes the result from the cache and skips the computation.
# If not , just calculates it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        matrix_to_invert <- x$get()
        inv <- solve(matrix_to_invert, ...)
        x$setinverse(inv)
        inv
}

# Execution:

Matrix1 <- makeCacheMatrix(matrix(10:13, 2, 2))
Matrix1$get()

Matrix1$getinverse()

cacheSolve(Matrix1)

cacheSolve(Matrix1)

Matrix1$getinverse()





