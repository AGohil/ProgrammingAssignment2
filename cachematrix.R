## The assignment has been based on the sample calculate mean function
## provided during the class

## The makeCacheMatrix function will take matrix as input and have a function
## to store the inverse of matrix in the parent environment

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- matrix()
  
  ## The set function takes a matrix as an input, and assigns it to X, and initializes
  ## the inv_matrix varibale to null
  set<-function(y){
    x <<- y
    inv_matrix<<- NULL
  }
  
  ## the get function returns the matrix passed as an input
  get <- function() x
  
  ## the setinv function stores the inverse of the matrix to the inv_matrix
  ## variable in the parent environment
  setinv <- function(inv) inv_matrix<<-inv
  
  ## the getinv reads the inverse of the matrix from the parent environment
  getinv <- function() inv_matrix
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## The cachesolve function searches the parent env to see
## if the inverse exists, and returns it if it is there.
## if the inverse does not exist, then it calculates the inverse using the
## solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix<-x$getinv() ## reads the parent environment to see if the inverse exists
  if (!is.null(inv_matrix)){
    message("getting cached data")
    return (inv_matrix)
  }
  
  newmatrix<-x$get() ## gets the matrix for which the inverse needs to be found
  inv_matrix<-solve(newmatrix) ## inverse matrix is calculated using the solve function
  x$setinv(inv_matrix) ## stores the inverse in the parent environment
  inv_matrix
}