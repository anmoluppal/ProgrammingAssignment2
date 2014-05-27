## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Initializes the cache
        
        m <- NULL
        
        # Stores the matrix for which inverse needs to be computed
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Returns the matrix
        
        get <- function() x
        
        # Sets the inverse value in the cache
        
        setmatrix <- function(solve) m <<- solve
        
        # Gets the inverse value from the cache
        
        getmatrix <- function() m
        
        ## Returning the list containing the 4 functions
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Gets the value of the matrix
        
        m <- x$getmatrix()
        
        # Checks if the inverse is available in the cache
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Computes the inverse of the matrix using the solve function
        
        matrix <- x$get()
        m <- solve(matrix, ...)
        
        # Sets the computed inverse value in the cache and returns it
        
        x$setmatrix(m)
        m
}
