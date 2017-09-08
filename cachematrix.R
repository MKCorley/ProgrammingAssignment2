## This function first creates a special "matrix" object that can cache its inverse. 
##Then, if the inverse has already been calculated, then cachesolve retrieves the inverse from the cache.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) { #set default value of x as an empty numeric vector. 
  m <- NULL #initiale an object within the makeCacheMatrix() environment to be used by later code in the function.
  set <- function(y) {
    x <<- y #Assign the input argument to the x object in the parent environment
    m <<- NULL #Assign the value of NULL to the m object in the parent environment. 
  }
  get <- function() x #defines the getter for x
  setinverse <- function(solve) m <<- solve #defines the setter for the inverse m.
  getinverse <- function() m #defines the getter for the inverse m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.
cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #retrieve an inverse from the object passed in as the argument
  if(!is.null(m)) { #check to see whether the result is NULL
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
