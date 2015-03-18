## Pair of functions to calculate and cache a inverse of a matrix rather than compute it continuously.

## This function creates a special "matrix" object that can cache its inverse.
## 1. Set the matrix value
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
## How it works:
## Retrieve (getinverse) the current value (NULL if not cached).  If it is not NULL then return that retrieved
## (cached) value.  If not found, get the data, solve the inverse, set the inverse (setinverse to cache it) and
## finally return the cached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv) # returning cahced value
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv # returning newly calculated value
}
