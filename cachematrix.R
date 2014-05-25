## Functions to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) 
{
    inverse <- NULL
    
    set <- function(value)
    {
        m <<- value
        inverse <<- NULL
    }
    
    get <- function() m
    
    setInverse <- function(value) inverse <<- value
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(cacheMatrix, ...) 
{
    inverse <- cacheMatrix$getInverse()
    if(!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    m <- cacheMatrix$get()
    m <- solve(m)
    cacheMatrix$setInverse(m)
    m
}
