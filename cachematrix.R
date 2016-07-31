## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

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
 


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It first checks if the inverse has already been calculated. 
## If so, the cachesolve retrieves the inverse from the cache and skips the calculation. If not, it calculates the inverse, sets the value in the cache with the setinverse function.
  
 cachesolve <- function(x, ...) {
          inv <- x$getinverse()
            if(!is.null(inv)) {
                       message("getting cached data")
                       return(inv)
               }
           data <- x$get()
             inv <- solve(data)
             x$setinverse(inv)
            inv
     }
  