## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    sm <- dim(as.matrix(y))
    if(length(sm)==2 & sm[1]==sm[2]){
      x <<- as.matrix(y)
      inv <<- NULL
    }
    else stop("input must be a square matrix")
  }
  get <- function() x
  setinv <- function(inverse){
    sm <- dim(as.matrix(inverse))
    if(length(sm)==2 & sm[1]==sm[2]) inv <<- as.matrix(inverse)
    else stop("input must be a square matrix")
  }
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse matrix")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m)
  x$setinv(inv)
  inv
}
