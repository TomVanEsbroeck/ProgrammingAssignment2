
##  10/05/2019 Tom Van Esbroeck (Tom.VanEsbroeck@globalpsa.com)
##  
##  Coursera - R Programming
##  Week 3 - Programming Assignment 2: Lexical Scoping

##  Assignment: caching the inverse of a matrix

##  makeCacheMatrix: This function creates a special "matrix" object that 
##  can cache its inverse. 
##  It's really a list containing functions to:
##  - Set the value of the matrix
##  - Get the value of the matrix
##  - Set the inverse of the matrix
##  - Get the inverse of the matrix

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

##  cacheSolve: This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been 
##  calculated (and the matrix has not changed), then the cachesolve should 
##  retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
