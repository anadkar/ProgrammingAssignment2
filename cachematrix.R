## The two functions below allow caching of the inverse of a matrix
## to allow for quick retrieval when the inverse of an unchanged matrix is 
## required.

## The makeCache Matrix function below returns an object that is a list of four
## functions to create a matrix, retrieve the matrix, calculate the inverse of 
## the matrix, cache it, and retrieve the inverse from the cache.
## Usage: m <- makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## Sets the inverse to NULL
        set <- function(y) {
                x <<- y  ## Writes matrix to 'x'
                m <<- NULL ## Sets the inverse in the cache to NULL
        } ## This function creates the matrix and sets its inverse to NULL
        
        get <- function() x ## Function to retrieve the matrix
        
        setinv <- function(solve) m <<- solve   ## Writes the inverse to cache
        
        getinv <- function() m  ## Allows retrieval of inverse from cache
        
        ## Returns a list of functions.
        
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        ## The functions can be used as follows ( The object 'm' has been defined
        ## previously) :
        ## m$set(matrix(rnorm(4), 2, 2))
        ## m$get()
        
}


## The cacheSolve function below checks to see if the inverse of the matrix
## being requested already exists. If so, it retrieves it from the cache using 
## the getinv() function along with a message regarding the same. If the inverse 
## does not already exist, it proceeds to calculate the inverse and uses the 
## setinv() function to cache it.

cacheSolve <- function(x, ...) {
        m <- x$getinv() ## Try to get inverse from cache
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } ## If inverse exists, return it along with a message
        
        ## otherwise get matrix, calculate inverse, cache it and return
        ## inverse
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
} 
        

