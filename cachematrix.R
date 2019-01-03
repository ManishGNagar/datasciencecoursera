## This program basically takes a matrix and inverts it. 
## It returns it from cache if nothing has changed and saves time by not recalculating everytime.
## I have added three lines to test the script

## This function returns four functions as a list. These are getter/setter functions
makeCacheMatrix <- function(x = matrix()) {
        nReturn <- NULL
        #define simple getter/setter functions
        #set function will assign the passed matrix to m
        set <- function(y) {
                x <<- y
                nReturn <<- NULL
        }
        #get function will return the matrix to be inversed
        get <- function() x
        #setinverse function will set the inversed matrix 
        setinverse <- function(mInverse) nReturn <<- mInverse
        #getinverse function will return inversed matrix
        getinverse <- function() nReturn
        #return all the functions defined as list
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## using getinverse function  get the value for inversed matrix
        nReturn <- x$getinverse()
        #check if it is availabke in cache, if so - return
        if(!is.null(nReturn)) {
                message("getting inverse from cached data")
                return(nReturn)
        }
        #if not in cache, create, set and return
        m <- x$get()
        nReturn <- solve(m, ...)
        x$setinverse(nReturn)
        nReturn
}

#Testing Script
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix <- makeCacheMatrix (m1)
cacheSolve(myMatrix)
cacheSolve(myMatrix)
