## The following functions provide the ability
##### to cache the time-consuming computation of the inverse matrix.
## This approach takes advantage of the scoping rules of
##### the R language to preserve state inside of an R object.
## EXAMPLE:
##### > x = matrix(1:4,2,2)
##### > b = makeCacheMatrix(x)
##### > cacheSolve(b)
#####      [,1] [,2]
##### [1,]   -2  1.5
##### [2,]    1 -0.5
##### > cacheSolve(b)
##### getting cached data
#####      [,1] [,2]
##### [1,]   -2  1.5
##### [2,]    1 -0.5
##### > x %*% cacheSolve(b)
##### getting cached data
#####      [,1] [,2]
##### [1,]    1    0
##### [2,]    0    1
##### >


## function makeCacheMatrix creates a special "matrix", which is
##### really a list containing a function to
########## 1.  set the value of the matrix
########## 2.  get the value of the matrix
########## 3.  set the value of the inverse matrix
########## 4.  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve calculates the inverse matrix of the special "matrix"
##### created with the above function. 
## However, it first checks to see if the
##### inverse matrix has already been calculated. 
## If so, it `get`s the inverse matrix from the
##### cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of
##### the data and sets the value of the inverse matrix
##### in the cache via the `setinv`function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
