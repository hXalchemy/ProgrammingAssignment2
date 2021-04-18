## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverted) minv <<- inverted
  getinverse <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted <- x$getinverse()
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data, ...)
  x$setinverse(inverted)
  inverted
}


#Please uncomment code below to validate the functions
# Ref: for matrix m2 https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html

#m1 = diag(3)
#solve(m1)

#m2 = rbind(c(3,0,2),c(2,0,-2),c(0,1,1))
#solve(m2)

#cmat1 <- makeCacheMatrix(m1)
#cacheSolve(cmat1)
#cacheSolve(cmat1) #should return cached

#cmat2 <- makeCacheMatrix(m2)
#cacheSolve(cmat2)
#cacheSolve(cmat2) #should return cached
