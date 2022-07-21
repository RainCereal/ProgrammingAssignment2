## makeCacheMatrix is function to a list with set,get,setSolve, and getSolve for a matrix
## cacheSove is a function to print the inverse of a matrix from the list generated in makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data :)") ##message with a smile
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

##test drive and outputs
j <- 1:4 #make vector
k <- matrix(j, 2, 2) #store a simple matrix
makeCacheMatrix(k) #check that a list is returned
l <- makeCacheMatrix(k) #store the list
> cacheSolve(l) #this returns the same value as solve(k), the inverse of matrix k
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(l)
getting cached data :) ##running it again give the message with a face, returns the cached inverse of matrix k
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
