## Two functions:
## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##    If the inverse has already been calculated (and the matrix has not changed),
##    then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix Ffnction creates a special "vector" or list to do the following:
## 1) Set the value of the vector by using the <<- operator which assign a value to an object in an environment
##    outside of the current environment.
## 2) Get the value of the vector
## 3) Set the value of the solve function (again using the <<-operator) to object m
## 4) Get the value of the solve function of object m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Assign makeCacheMatrix to a special variable m first, so that the variable retains access to the entire environment defined 
## by the makeCacheMatrix(), and also all the functions included in the list, including the get() and set() functions
## example code below passes a matrix vector called "vec"
m <- makeCacheMatrix(vec)

## cacheSolve function:
## Checks to see if the a cached matrix has already been calculated.  If so, then it gets the matrix from the cache and skips
## the computation.
## Otherwise, it calculates the inverse of the matrix vector and sets the value of the mean in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## Note that running cacheSolve with the special variable m allows us to use the get() and set() functions from the makeCacheMatrix function.
cacheSolve(m)