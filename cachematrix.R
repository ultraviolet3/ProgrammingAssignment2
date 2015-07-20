# Commits by uma venkataranmani
#This function defines thew functions to set the value for the matrix and another to get retrieve the value of the matrix, set the inverse of the values in the matrix and to retrieve them.

makeCacheMatrix <- function(x = matrix()) {
  xcac <- NULL 
  xcac <<-NULL # initializing xcac that stores the cache to null in the global env
 
 set <- function(y) 
    x <<- y  # assigning the matrix y to x in the global env
    xinv <<- NULL # initializing xinv that store the inverse value of x to null in the global environment
 
    get <- function() x # defining the get function to retrieve the matrix stores in x in the global environment
    setinv <- function(inv) xinv <<- inv # passing the matrix variable in the global environment to the local variable to the function to calculate the inverse
    getinv <- function() xinv # retrieving the inverse after it has been calculated
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# This function computes the inverse of the matrix values provided the inverse has not be stored in the cached variable in the global environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ginv <- x$getinv()
  if (!is.null(xinv)) {
    message("getting cached data")
    return(ginv)
    
  }
  retinv <- x$get()
  xinv <- solve(retinv)
  x$setinv(xinv)
  xinv
  
}
# Sample
#> z<-matrix(c(1:4),2,2)
#> zm<-makeCacheMatrix(z)
#> zm$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(zm)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(m)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
 