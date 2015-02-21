## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
        x <<- y
        m <<- NULL
}
     
 get <- function()x
   setmatrix <- function(solve) m <<- solve
   getmatrix <- function() m
   list(set=set, get=get, 
   setmatrix=setmatrix, 
   getmatrix=getmatrix)
      
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
   m <- x$getmatrix()
   if(!is.null(m)) {
        message("getting cached data")
        return(m)
   }
   
   matrix <- x$get()   
## runs the getmatrix function to get the value of the input matrix
   m <- solve(matrix,  ...)
   x$setmatrix(m)
## run the setmatrix function on the input matrix to cache it
   m
## Return a matrix that is the inverse of 'x'
}


