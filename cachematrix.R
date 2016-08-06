## Caching the inverse of a Matrix:
## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than computing it reapetedly.
## Below are a pair of functions used to create a special
## object storing a matrix and caches its inverse

## This function creates a special "matrix" object which caches
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
   set <- function(y) {
     x <<- y
     inv_x <<- NULL
  }
   get <- function() x
   setinverse<- function(inverse) inv_x <<-inverse
   getinverse <- function() inv_x
   list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## created by the makeCacheMatrix above. If the inverse has 
## already been calculated and the matrix has not changed, 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
       if (!is.null(inv_x)) {
             message("getting cached inverse matrix")
             return(inv_x)
         } else {
           inv_x <- solve(x$get())
           x$setinverse(inv_x)
           return(inv_x)
           }
}
