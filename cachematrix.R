## These two functions together cache a matrix and its inverse


## Creates matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInver <- function(InMatrix) inver <<- InMatrix
  getInver <- function() inver
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
  
}


## Computes the inverse of the matrix created by "makeCacheMatrix" (will not compute
##inverse of matrix that was not run through "makeCacheMatrix")

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInver()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setInver(inver)
  inver      
}
