## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  var1 <- NULL
  
  set <- function(y) {
    x <<- y
    var1 <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) var1 <<- inverse
  getinverse <- function() var1
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x) {
  var1 <- x$getinverse()
  if(!is.null(var1)) {
    message("getting cached data.")
    return(var1)
  }
  data <- x$get()
  var1 <- solve(data)
  x$setinverse(var1)
  var1
}

## let's try it

## 1st, we need a matrix
## MAT <- matrix(c(1,3,5,7),2,2)

## iMAT <- makeCacheMatrix(MAT)
## cacheSolve(iMAT)
##       [,1]   [,2]
##[1,] -0.875  0.625
##[2,]  0.375 -0.125

## cacheSolve(iMAT)
## getting cached data.
##        [,1]   [,2]
## [1,] -0.875  0.625
## [2,]  0.375 -0.125