#                                       R PROGRAMMING
#                                 PROGRAMMING ASSIGNMENT 2
# 
# Write the following functions:
#         
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the solve function in R. For example, 
# if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.


# Parte 1: makeCacheMatrix: creates the special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # this defines the argument of the function as an empty matrix
  
  # Set the inverse of the matrix equal to NULL
  Inv <- NULL
  
  set <- function(y){
    
    # We need this function to assign its argument to x
    x <<- y
    
    # Once the set function is called, Inverse needs to be redefined to NULL 
    Inv <<- NULL
    
  }
  
  #Now we need the matrix to be returned:
  get <- function() {
    x
  }
  
  #The inverse is defined:
  setInv <- function(solve) {
    Inv <<- solve(x)
  }
  
  #The inverse of the matrix is now returned:
  getInv <- function() {
    Inv
  }
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
  # creates a list of the functions
}

# Part 2: 
#cacheSolve: This function checks if the inverse of the matrix has already been 
#calculated. If so, and if the matrix has not changed, the cachesolve should retrieve the 
#inverse from the cache. If not, it computes the inverse of the special "matrix" returned by 
#the previous function, makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  
  # Retrieves the most recent value for the inverse
  Inv <- x$getInv()
  
  # If the inverse is NOT null because it has already been calculated, cacheSolve 
  # returns it:
  
  if(!is.null(Inv)){
    message("Getting cached data...")
    return(Inv)
  }
  
  # If the inverse is NULL (it hasn't been calculated), then we need to get matrix x 
  # and calculate the inverse with the solve() function:
  else {
    
    #Print message:
    message("newly calculating data")
    
    #We assign a new name to the matrix:
    mat <- x$get()
    
    #The inverse of the matrix is defined:
    Inv <- solve(mat, ...)
    
    x$setInv(Inv)
    # Sets Inverse to the newly calculated value
  }
  
  Inv #Returns the new Inverse value
  
}
##_______________________________________##
##example:
## we create a invertible matrix of 2x2
c<-matrix(c(1,1/4),c(1/4,1))
## Now we call the makeChacheMatrix function
kaka=makeCacheMatrix(c)
kaka$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00
##____________________________________________________##
##The first run whitout cached data
> cacheSolve(kaka)
newly calculating data
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667
##______________________________________________________________##
##now retrieving the data in the cache
kaka$getInv()
> print(kaka2)
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667
## solve the matrix whit cached data:
> cacheSolve(kaka)
##_______________________________________________________________________##
##Getting cached data...
> cacheSolve(kaka)
Getting cached data...
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667
##______________________________________________________________________________##
