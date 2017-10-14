## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# set: define the matrix value in the environment
# get: method to access to the value of matrix in the environment
# setInverse: define the value of inverse of the matrix in the environment
# getInverse: method to access to the matrix inverse value in the environment
makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y){
        x <<- y
        invX <- NULL
    }
    get <- function() x
    setInverse <- function(invMat) invX <- invMat
    getInverse <- function() invX
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Write a short comment describing this function
#function that retruns the matrix inverse
#Initialisation of the variable invX
#Verification of the existence of this variable in the environment
#Initialisation of the local variable localX
#using solve function to calculate the matrix inverse
#return of the inverse of matrix

cacheSolve <- function(x, ...) {
    invX <- x$getInverse()
    if(!is.null(invX)){
        message("getting cached data")
        return(invX)
    }
    localX <- x$get()
    invX <- solve(localX)
    x$setInverse(invX)
    invX
}


#results
# x = matrix(c(1,3,5,7,1,11,13,17,1),nrow = 3,ncol = 3,byrow = TRUE)
# y = makeCacheMatrix(x)
# y$get()

#     [,1] [,2] [,3]
#[1,]    1    3    5
#[2,]    7    1   11
#[3,]   13   17    1

#cacheSolve(y)
#           [,1]        [,2]        [,3]
#[1,] -0.2473404  0.10904255  0.03723404
#[2,]  0.1808511 -0.08510638  0.03191489
#[3,]  0.1409574  0.02925532 -0.02659574

# z <- cacheSolve(y)
# z%*%x
#              [,1]          [,2]          [,3]
#[1,]  1.000000e+00  0.000000e+00 -1.457168e-16
#[2,]  1.665335e-16  1.000000e+00 -2.012279e-16
#[3,] -5.551115e-17 -5.551115e-17  1.000000e+00
