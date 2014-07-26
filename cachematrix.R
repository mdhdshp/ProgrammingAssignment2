## This function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()){
  i<-NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated and the matrix has not changed,then 
## cacheSolve retrieves the inverse of the matrix from cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        
}

## Below is the sample execution for the above R code
a<-matrix(c(3,4,5,6),2,2)
b<-makeCacheMatrix(a)
cacheSolve(b)
cacheSolve(b)
