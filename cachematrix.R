## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

##The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean

## The second function calculates the Inverse of the special "matrix"
## created with the first function. It first checks to see if the
## Inverse has already been calculated. If so, it `get`s the Inverse from the
## cache and skips the computation. Otherwise, it calculates the Inverse of
## the matrix and sets the value in the cache via the `setInverse`function.

## makeCacheMatrix: this function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve`: This function computes the inverse of the special 
##"matrix" returned by `makeCacheMatrix` above. If the inverse has already 
## been calculated (and the matrix has not changed), then `cacheSolve` should
## retrieve the inverse from the cache.Write a short comment describing this 
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I <- x$getInverse()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    I
}
