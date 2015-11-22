## The first of the following 2 functions (makeCacheMatrix) is a function 
## which consists of a list of 4 functions that store a matrix and its
## inverse. 
## The second function (cacheSolve) calculates the inverse (if it hasn't 
## already been calculated) and stores it in a function  that was created
## with the "makeCacheMatrix" function. 

## A) makeCacheMatrix is a function that creates a special "matrix",
## which is really a list containing 4 functions each of them doing 
## the following things:
## 1.set the values of the matrix (x), (set)
## 2.get the values of the matrix (x), (get)
## 3.set the values of the inversematrix (i), (setinverse)
## 4.get the values of the inversematrix (i), (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## B) The cacheSolve function checks if the inverse matrix has
## already been calculated by the previous function and is not NULL.
## If the inverse matrix has been calculated, then it stops and returns
## a message ("getting cached data") and the stored matrix. If it has not,
## then it calculates it, stores it in the setinverse function (part of 
## the makeCacheMatrix function)and prints it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}