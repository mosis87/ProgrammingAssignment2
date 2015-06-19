# Overall this function allows to caching the inverse of a matrix rather than compute it repeatedly.
# The makeCacheMatrix creates a special "matrix" object that can cache its reverse. Then, cacheSolve 
# checks if the inverse has already been calculated, it retrieves and return it. If not, it calculates,
# caches and returns the inverse of the matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                  inv <- NULL
                  set <- function(y) {
                          x <<- y
                          inv <<- NULL
                  }
                  get <- function() x
                  setinv <- function(solved) inv <<- solved
                  getinv <- function() inv
                  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# This function calculates the inverse of a matrix using a special "matrix" created by makeCacheMatrix.
# If the inverse has already been calculated and the matrix has not changed, the function 
# retrieves it from the cache. If not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
                  inv<- x$getinv()
                  if (!is.null(inv))  {
                          message("getting cached data")
                          return(inv)
                  }
                  data <- x$get()
                  inv <- solve(data)
                  x$setinv(inv)
                  inv
}
