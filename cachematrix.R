## The two functions presented here have for objective to cache the inverse of a matrix.
## The "solve" function, which calculates the inverse of a matrix, 
## can be very time-consuming for large matrices, so caching the inverse
## will allow us to save precious time whenever this function gets called more than once on the same matrix

## The following function, makeCacheMatrix, create a lists containing a function to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setM <- function(y) {
                x <<- y
                S <<- NULL
        }
        getM <- function() x
        setsolve <- function(mean) S <<- solve
        getsolve <- function() S
        list(setM = setM, getM = getM,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function, cacheSolve, calculates the inverse of the special "matrix" created with the above function.

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

# ExampleMatrix <- matrix(rnorm(10^6),10^3,10^3)
# M <- makeCacheMatrix()
# M$setM(ExampleMatrix)
# Result <- cacheSolve(M)
# sum(Result == solve(ExampleMatrix)) == length(ExampleMatrix) # Should return TRUE if both functions are correct
