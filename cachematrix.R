## put a matrix invertion in cache and take it again

## create a special "vector" which is a list to the function set, get, setinvmat,getinvmat

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) m <<- invmat
  getinvmat <- function() m
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## this function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmat(m)
  m
}
