## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Write a short comment describing this function
## makeCacheMatrix takes a matrix as an argument and sets the value of "m" to NULL
## set() function stores the argument matrix and the vaue of m in a different environment
## get() function is the getter for the vector x which retrives matrix from parent environment
## of makeCacheMatrix()
## setinv() is the setter for the inverse matrix "inv" 
## list assigns get, set, getinv, setinv functions as elements within a list, and returns it to the parent environment.
## makeCachematrix returns fully formed object to be used by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## cacheSolve() takes the object returned from 
## makeCacheMatrix() as input
## Then it retrives the value of m and checks if m is NULL
## if m contains the inverse matrix, it returns the value of m without recalculating inverse matrix
## if m is null, the function retrives the matrix bu "x$get()" function
## and assigns inverse matrix to m and returns m


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
         m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
