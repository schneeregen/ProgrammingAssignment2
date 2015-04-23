## The functions makeCacheMatrix and cachSolve take a square matrix and compute the 
## inverse of the matrix. If the computation has already been done, only a cached version 
## will be shown. 


## The makeCacheMAtrix function is a list of functions. It can store a vector and cache
## the inverse of the vector (delivered by cacheSolve). For both functions it uses 
## the <<- operator which assigns a value to an object in an environment that is 
## different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # i stands for "inverse Matrix"
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setmatrix <- function(inverse.result) i <<- inverse.result  #caching
    
    getmatrix <- function() i
    
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## CacheSolve checks if an inverse matrix is cached and else computes it using the solve
## function

cacheSolve <- function(x, ...) {
    i <- x$getmatrix()  # getting i (which can be NULL or a cached inverse matrix)
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    matrix <- x$get()
    
    i <- solve(matrix, ...)  # computing the inverse
    
    x$setmatrix(i)  # call of the caching function
    
    i
}
