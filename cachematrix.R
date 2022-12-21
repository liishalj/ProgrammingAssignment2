## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## initialize objects
  m <- NULL
  set <- function(y) { ## define setters and getters
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, ## create a new object by returning a list
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## retrieve the inverse
  if(!is.null(m)) { ## check whether the result is NULL
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## if the result of !is.null(m) is FALSE, 
  m <- solve(data, ...) ## then cachemean() calculates the inverse
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
