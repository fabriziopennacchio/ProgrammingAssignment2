## This is a cache function for calculating the inverse of a Matrix

## In make cache matrix the "special matrix" object is created and stored

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y) {
  x<<-y
  inv<<-NULL
}
get<-function() x
setinv<- function(inverted) inv<<-solve
getinv<-function() inv
list(set=set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## In cacheSolve the inverse matrix is calculated ex novo if the matrix in input is new, otherwise it returns the already computed and stored inverse matrix

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()

  inv <- solve(data, ...) 
  x$setinv(inv)
  inv
  
        ## Return a matrix that is the inverse of 'x'
}

