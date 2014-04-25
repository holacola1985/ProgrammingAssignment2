## Put comments here that give an overall description of what your
## functions do
# The first function, makeCacheMatrix() create a special vector, whch is a list containing
#       the functions that could read and cache the existing inverse matrix
# The second function, cacheSolve() will compute the inverse matrices which are not cached 
#       in the makeCacheMatrix().

## Write a short comment describing this function
# this function contains four functions:
#       1.set the value of the matrix
#       2. get the value of the matrix
#       3. set the value of the inverse
#       4. get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
# this function compute the inverse matrix which has not been cached from above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
