##The following 2 functions are used to create a special object 
##that stores a matrix and cache's its inverse.

##The following function creates a matrix which is a part of
##a list containing 4 functions to:
##1 initialize the matrix
##2 get the values of matrix
##3 set the value of matrix inverse "inv"
##4 get the value of matrix inverse "inv"
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##The following function creates the inverse of the matrix
##created in the above function. However, it checks to see 
##if the inverse has already been calculated. When there is 
##a valid inverse value it skips the calculation and returns
## the stored/cached value of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}