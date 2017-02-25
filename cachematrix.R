## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function manages the caches

makeCacheMatrix <- function(x = matrix()) {
##
      inv <- NULL
      #message(inv)
      set <- function(im) {
        x <<- im
        message('in set function')
        inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
#cacheSolve checks to see if the argument matrix is identical to the previous
#argument supplied  - if it is not identical it calculates the inverse of the matrix
#                   - if it is identical it retrieves the cached inverse
cacheSolve <- function(x, ...,newZ) {
    data <- x$get()
      if(identical(data,newZ)==TRUE){
        message("getting cached data")
        inv <- x$getinv()
        return(inv)
      } else if(identical(data,newZ)==FALSE){
        message("recalculating matrix inverse")
        inv <- solve(newZ, ...)
        x$set(newZ)
        x$setinv(inv)
        return(inv)
      }
}
