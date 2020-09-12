## Create an improved matrix object that can store it's own inverse,
## and will return this stored value rather than recalculating it if
## the matrix has not changed

## An example square matrix: 
## mymatrix <- matrix(c(1,1,3,0,2,4,-1,1,0),nrow=3,byrow=TRUE)

## Create a special matrix object that can store it's own inverse
## if it has already been calculated. It has functions set, get
## which set or get the matrix itself and setinverse, getinverse
## which set or get the inverse

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function () x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function () m
   list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## Calculate the inverse of a matrix. If the inverse of that
## matrix has already been calculated return the stored result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if (!is.null(m)) {
         message("getting cached data")
         return(m)
   }
   data <- x$get()
   m <- solve(data,...)
   x$setinverse(m)
   m
}
