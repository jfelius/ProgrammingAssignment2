## 21 January 2015.
## The two functions 'makeCacheMatrix' and 'cacheSolve' use lexical scoping properties in order to
## provide matrix inversion while keeping track of the results, so that once a matrix has been inverted,
## the inverse matrix will be retrievable from cache and does not need to be determined again.


## 'makeCacheMatrix' takes as formal argument the matrix 'x' (assumed to be invertible), and
## generates a list object, which contains 4 elements of type function: $set(), $get(), $setInverse(),
## and $getInverse(). These may be used to store the original matrix 'x' to cache, retrieve 'x' from
## cache, store the inverse of 'x' to cache, and retrieve the inverse of 'x' from cache, respectively.

makeCacheMatrix <- function(x = matrix()) {
      inverse.x <- NULL
      set <- function(y) {                  
            x <<- y                         
            inverse.x <<- NULL                   # <-- this resets the cached inverse back to NULL whenever
      }                                          #     $set() is used to change the contents of 'x'.
      get <- function() x
      setInverse <- function(inverse) inverse.x <<- inverse
      getInverse <- function() inverse.x
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}
## NOTE: Any future changes to the original matrix 'x' will need to be made by using the
## function $set(), which will set the new value AND flush the cached 'old' value of the inverse.



## 'cacheSolve' takes as argument a list of functions produced by 'makeCacheMatrix', and uses those
## functions to retrieve the inverse of the original matrix if it was previously cached. If the inverse
## was not previously cached, then the inverse will be calculated regularly (using solve()) and then
## stored to cache.

cacheSolve <- function(x, ...) {
      inv.x <- x$getInverse()               # $getInverse() retrieves the previously calculated inverse, or ...
      if(!is.null(inv.x)) {
            message("getting cached data")
            return(inv.x)
      }
      data <- x$get()                       # if $getInverse() returned NULL, $get() looks up the original matrix,
      inv.x <- solve(data, ...)             # .. determines its inverse,
      x$setInverse(inv.x)                   # .. then stores it by using $setInverse.
      inv.x                                 # returns the inverse.
}
