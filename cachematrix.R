# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makecachematrix <- function(x = matrix()) 
{
  inv <- NULL 
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} 
  setinverse <- function(solve) {inv <<- solve} 
  getinverse <- function() {inv}  
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve(x)
## return the cached inverse of matrix if the inverse had been calculated and
## its input matrix doesn't change. Recalculate the inverse matrix if the inverse
## has not calculated or the input had changed since last invocation. 

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse() 
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}