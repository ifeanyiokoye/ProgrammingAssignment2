## The functions below are used to cache the inverse of a matrix
# Assumes that the matrix is invertible
# These functions are potentially time saving 

makeCacheMatrix <- function(x = matrix()) {
        
        # makeCacheMatrix creates a list containing a function to
        # 1. set the value of the matrix
        # 2. get the value of the matrix
        # 3. set the value of inverse of the matrix
        # 4. get the value of inverse of the matrix
        ## The inverse of the matrix is then passed to the next function cacheSolve()
        
        inv <- NULL
        set <- function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        # Define functions to get and set the inverse in the cache
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}



# This function returns the inverse of a matrix after determining 
# if the inverse of the matrix has been computed. If it has been
# it gets the result and skips the computation
# else, it computes the inverse and sets the calue in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        # Checks if the inverse has already been calculated
        if(!is.null(inv)) {
                # Gets the calculated inverse from the cache and skip the computation
                message("getting cached data")
                return(inv)
                
        }
        # Calculates inverse if not already calculated
        data <- x$get()
        inv <- solve(data, ...)
        
        # Sets the value of the calculated inverse in the cache 
        x$setinverse(inv)
        inv
}
