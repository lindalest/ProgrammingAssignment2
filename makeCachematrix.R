
# makeCacheMatrix creates a special matrix object that can cache its inverse 
# CacheMatri can return a list of function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #sets the value of inv to Null, provides a default if cacheSolve has not been used
  set <- function(y) {
    x <<- y #caches the inputted matrix so the cacheSolve can check for changes
    inv <<- NULL
    
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve #caches matrix for future use
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve: 
# if the inverse has already been calculated then the cachesolve should retrieve the 
# inverse from the cache 

cacheSolve <- function(x, ...){
  inv <-x$getinv()
  if(!is.null(inv))  {
    message("getting cached data")
  return(inv)
  }
  data <-x$get()  #obtain cached data
  inv <-solve(data, ...)  #perform inverse on cached data
  x$setinv(inv)  # cache the inverse
  inv   # Return inverse matrix
  }
  
   
