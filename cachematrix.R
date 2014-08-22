## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation. Through this programme we will see the benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing 
## a function to
##    1. set the matrix
##    2. get the matrix
##    3. set the inverse of the matrix
##    4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  y <- NULL 
  
  setmatrix <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
  
  getmatrix <- function()  {x} 
  
  setinverse <- function(solve) 
          {m <<- solve}
  
  getinverse <- function() {m}
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse, getinverse = getinverse)
  
}



## The following function cacheSolve calculates the inverse of the special "matrix" created with the above
## function. However, it first checks to see if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and  
## sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function (x, ...) {
  
  m <- x$getinverse() 
  
  if(!is.null(m)){ 
    
    message("getting cached data")
    m <- x$getinverse()
    
    return(m)
  }
  
  # otherwise 
  data <- x$getmatrix() 
  x$setmatrix(data)     
  
  m <- solve(data)   
  x$setinverse(m)    
  m                  
  
}

