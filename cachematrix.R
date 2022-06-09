## Put comments here that give an overall description of what your
## functions do: This is an example of the difference in time it takes to use a cached version of a function vs
# running the same function over and over again. The first function collects the data and the second one runs 
# and measures the calculations

## Write a short comment describing this function: The function is makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse. 

#This returns a list of data containing functions to set and get the matrix.
#It also sets and gets the inverse. This data is used in cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}




## Write a short comment describing this function:
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# This function contains data from the previous function like: set the matrix, get the matrix
#set the inverse, get the inverse. It then runs the computation and measures the time of using a cached version vs
#running it over and over again.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inv = x$getinv()
  
  
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}



