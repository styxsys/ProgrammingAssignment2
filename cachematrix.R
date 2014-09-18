## The function "makeCacheMatrix" create a factor has 2 matrixes and 4
## functions

## The matrix "x" is the original arguement matrix while matrix "inv" is the
## inverse matrix of x

## The functions aim to get and set values of the 2 matrixes

## Function "cacheSolve" calculates "inv" for the factor created by
## "makeCacheMatrix"

## This function creates a special "matrix", which is really a list containing
## a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## If the factor already has the value of "inv", it gets the inverse matrix
## from the cache and skips the computation
## Otherwise, it calculates the inverse matrix of "data" and sets the value of
## the inverse matrix in the cache via the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
