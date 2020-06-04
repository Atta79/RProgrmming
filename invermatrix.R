makeCacheMatrix <- function(x = matrix()) { 
#Iniilaize inverse matrix   
inv <- NULL 
 set <- function(y) { 
  x <<- y 
    inv <<- NULL 
   } 
#return with matrix
   get <- function() x 
#inverse matrix
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv 
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
  } 
# Computation of inverse matrix by cache
cacheSolve <- function(x, ...) { 
      ## Return a matrix that is the inverse of 'x' 
   inv <- x$getinv() 
  if(!is.null(inv)) { 
     message("getting cached result") 
   return(inv) 
  } 
  data <- x$get() 
   inv <- solve(data, ...) 
   x$setinv(inv) 
   inv 
  } 
# To check wheather the programe is exected correctly
m <- matrix(rnorm(25),5,5)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)