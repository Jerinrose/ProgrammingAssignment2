
##This function will create a matrix object which caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveinversematrix) inv <<- solveinversematrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##This function finds the inverse of the matrix returned by makeCacheMatrix above
##It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                 message("getting cached data")
                 return(inv)
         }
         data <- x$get()
         inv <- solve(data)
         x$setInverse(inv)
         inv      
}
