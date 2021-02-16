makeCacheMatrix <- function(x = matrix()){   ##This function creates a matrix object that can catche its inverse
matrixinv <- NULL #initialize matrixinv as null. This will eventually hold the value of the matrix inverse
set <- function(y){ #define the set function to assign new value of matrix in parent environment
  x<<- y  
  matrixinv <- NULL #Reset matrixinv value to null if there is a new matrix
}
get <- function() x  #The get function defined here returns the value of the matrix argument
setinverse <- function(inverse) matrixinv <<- inverse  #This assigns the value of matrixinv in parent environment
getinverse <- function()matrixinv  #This retrieves the value of matrixinv where called
list(set = set, get = get,  #This is required in order to refer to the functions with the $ operator
     setinverse = setinverse,
     getinverse = getinverse)
}
CacheInverse <- function(x,...){  #This function computes the inverse of the matrix returned by makeCacheMatrix above 
  matrixinv <- x$getinverse()
  if(!is.null(matrixinv)){
    message("getting cached data")
    return(matrixinv)
  }
  data <- x$get()
  matrixinv <- solve(data,...)
  x$setinverse(matrixinv)
  matrixinv
}