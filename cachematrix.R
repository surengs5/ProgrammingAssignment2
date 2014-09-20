## makeCacheMatrix: Objective is to produce a special vector with a list of functions to operate on a matrix
## cacheSolve: Objective is to provide a cached value of inverted matrix when available otherwise calculate and return
makeCacheMatrix <- function(x = matrix(...)) {
     ## Initialize with a NULL inversed matrix and in set function as well, so that new inverse gets calculated in cacheSolve
     m <- NULL
     set <- function(y) {
          x <<- y;  m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     
     ## Create a list of functions, which can be individually invoked
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
     m <- x$getinverse()

### Before returning cached inverse, with below condition check for existence of inverse and original matrix has not been changed (the source for calculated inverse matrix)
### if(is.matrix(m) && identical(m, solve(x$get()))) {
### if(is.matrix(m)) {     this check may not be fully enough, as original matrix can be changed by invoking setinverse with different values, after calculation of inverse

     if(is.matrix(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()

     ## No Cached Inverse Matrix exists, calculate inverse
     ## Check matrix is not scalar:  if (round(det(data), digits = 5) == 0) {print("Matrix is scalar, inverse can not be calculated"); return NULL}
     ## Above check commented out assuming matrix is invertible per assignment instructions
     m <- solve(data)
     x$setinverse(m)
     m
}
