## Functions in order to control a matrix and calculate it the "solve"

## Creates a special "matrix" object that can cache its inverse.
## @param x - Is the matrix with the which will create the obj
## @return set - Function that sets a new matrix in the obj
## @return get - Function that gets the  matrix loaded in the obj
## @return setCacheSolve - Function that loads the solve on cache
## @return getCacheSolve - Function that gets the solve on cache

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setCacheSolve <- function(solve) s <<- solve
  getCacheSolve <- function() s
  list(set = set, get = get,
       setCacheSolve = setCacheSolve,
       getCacheSolve = getCacheSolve)
}


## Computes the inverse of the special "matrix"
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should  from the cache.
## @param x - Is a obj. produced by makeCacheMatrix()
## @return - retrieve the inverse of the matrix.

cacheSolve <- function(x, ...) {
  s <- x$getCacheSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setCacheSolve(s)
  s
}

##Code for getting a matrix with inverse
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
matrixTest <- hilbert(8)

##Example of use

##source("cachematrix.R")
##m1 <- makeCacheMatrix(matrixTest)
##First. Getting -solve- by calculation
##cacheSolve(m1)
##Second. Getting -solve- by cache
##cacheSolve(m1)