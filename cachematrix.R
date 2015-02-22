
#makeCacheMatrix creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #sets the value of m to Null, provides a default if cacheSolve has not been used
  set <- function(y) {
    x <<- y #caches the inputted matrix so the cacheSolve can check for changes
    m <<- NULL
    
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
# write a function that creates a special matrix object that can cache its inverse 
# if the inverse has already been calculated then the cachesolve should retrieve the 
# inverse from the cache 

cacheSolve <- function(x,matrix, ...){
  m <-x$getmatrix()
  if(!is.null(m))
    message("getting cached dats")
  return(m)
  
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
