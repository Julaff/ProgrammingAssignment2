## The two functions presented here have for objective to cache the inverse of a matrix.
## The "solve" function, which calculates the inverse of a matrix, 
## can be very time-consuming for large matrices, so caching the inverse
## will allow us to save precious time whenever this function gets called more than once on the same matrix


## The following function, makeCacheMatrix, creates a list containing a function to :
## 1. makeCacheMatrix$SetM sets the value of the matrix into the cache
## 2. makeCacheMatrix$getM gets the value of the matrix from the cache
## 3. makeCacheMatrix$setsolve sets the value of the inverse of the matrix into the cache
## 4. makeCacheMatrix$getsolve gets the value of the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
        S <- NULL
        setM <- function(y) {
                x <<- y
                S <<- NULL
        }
        getM <- function() x
        setsolve <- function(solve) S <<- solve
        getsolve <- function() S
        list(setM = setM, getM = getM,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function, cacheSolve, returns the inverse of the special "matrix" created with the above function.
## The function follows these steps :
## 1. Looks into the cache to see if the inverse has already been calculated
## 2. If it has, exists the function and returns it, with a display message
## 3. Otherwise it gets the matrix from the cache, and calculates the inverse
## 4. Finally the inverse gets set into the cache to prevent making this long calculation a second time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        S <- x$getsolve()
        if(!is.null(S)) {
                message("getting cached data")
                return(S)
        }
        data <- x$getM()
        S <- solve(data, ...)
        x$setsolve(S)
        S
}

## Example of Execution (remove the comments)

# ExampleMatrix <- matrix(rnorm(9),3,3)
# M <- makeCacheMatrix()  # we get our list of functions ready
# M$setM(ExampleMatrix)   # we set the matrix into the cache
# cacheSolve(M) # the inverse is calculated a first time
# cacheSolve(M) # the second, the inverse is taken from the cache
