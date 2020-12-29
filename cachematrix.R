## This code consist of two functions, makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##cacheSolve:computes the inverse of the special "matrix" returned by makeCacheMatrix




makeCacheMatrix <- function(x = matrix()){
        inv <- NULL    # baseline inverse set as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x # gets the matrix x
        setInverse <- function(inverse){inv <<- inverse}
        getInverse <- function() {inv} # Returns the inverse of the function
        
        list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function (x, ...){ 
        inv <-x $ getInverse()
        if(!is.null(inv)){ # checks whether the inverse is null
                message("gtting cached data")
                return(inv) # returns the value of the inverse
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}