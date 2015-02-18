## These functions help trimming code runtime by caching inverse matrix values
## Part 1 (makeCacheMatrix) creates a matrix and stores the cache value
## Part 2 (cacheSolve) checks if there is a cache value, and if there isn't, it
## creates the inverse of the matrix


## This is part 1 - Creates an empty matrix, "m" and "x" become global variables
## for future use
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Part 2 - Evaluates if there is already cached data, if not, solves the matrix
cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}