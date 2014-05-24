## There are 2 main functions in this file. The first (makecachematrix) stores 
## the inverse of a matrix in cache. The second (cacheSolve) determines if the inverse
## of the Matrix is already calculated, if yes then returns it from cache else it calculates
## and caches it

 
## the makeCacheMatrix function caches the invers of a matrix given in a paramater

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


## the cacheSolve fuction inverts a matrix (created by makeCacheMatrix function)
## If the inversion of the given matrix is already calculated 
## then is will be returned from cache

cacheSolve <- function(x, ...) {
         ##Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         m <- solve(data)
         x$setinverse(m)
         m
}