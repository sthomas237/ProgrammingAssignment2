## The name of this file is cachematrix.R

## Coursera, R Programming, Peng, Leek, Caffo, June 2014
## Programming Assignment 2: Caching the Inverse of a Matrix
## Scott D. Thomas, http://www.linkedin.com/in/sthomas237

## The two functions in this R script work together to cache a matrix inverse.
## They are named makeCacheMatrix and cacheSolve.

## makeCacheMatrix puts an invertible square matrix into persistent storage.
## The object that it returns is a list of functions used by cacheSolve.
## In order to get the inverse from the cache matrix object, apply cacheSolve
## to it.  If the inverse matrix has been computed and cached, don't compute
## it again, but if it is stale (that is, NULL) then use solve to find it
## and return it.

## CAVEAT: for the assignment we assume the matrix is square and invertible.

## To test the two functions in this R script, for example:

## > source("cachematrix.R")
## > amat <- matrix(c(1,1,1,1,0,1,1,1,0,0,1,1,0,0,0,1),4)
## > cm <- makeCacheMatrix(amat)
## > ainv <- cacheSolve(cm)
## > amat %*% cacheSolve(cm)
## getting cached data
##      [,1] [,2] [,3] [,4]
## [1,]    1    0    0    0
## [2,]    0    1    0    0
## [3,]    0    0    1    0
## [4,]    0    0    0    1

## First a cm (cache matrix) object is formed on matrix amat, then the inverse
## ainv is formed and cached in cm, and then observe that the second cachSolve
## call returns cached data. You can see that the inverse is correct by matrix
## multiplication because the result is the identity matrix.

## Function descriptions and their definititions follow.

## makeCacheMatrix puts a matrix into persistent store also initializes its
## inverse to NULL to indicate that it has not been computed yet.  It then
## returns a list of functions mostly for use by the next function, cacheSolve

makeCacheMatrix <- function(a = matrix()) { ## a, an invertible square matrix
  ai <- NULL                                ## ai, inverse of a, initially NULL
  set <- function(b) {                      ## Change a to take the value of b
    a <<- b                                 ## Reset a, a persistent property
    ai <<- NULL                             ## When a changes, re-NULL ai
  }
  get <- function() a                       ## get, setsolve, and getsolve
  setsolve <- function(solve) ai <<- solve  ## are utility methods to be
  getsolve <- function() ai                 ## used in the cacheSolve function.
  list(set = set, get = get,                ## The cache matrix object that
       setsolve = setsolve,                 ## is returned by makeCacheMatrix
       getsolve = getsolve)                 ## is a list of functions.
}

## cacheSolve takes the object created above (cm, a cache matrix object) and
## either returns the inverse of the matrix stored there, or else computes
## the inverse of that matrix, puts it into persistent storage (cache), and
## then returns the inverse of the matrix.  No check for singular matrix.

cacheSolve <- function(cm, ...){            ## cm is a cache matrix object
  ai <- cm$getsolve()
  if(!is.null(ai)) {                        ## If the cached inverse is not
    message("getting cached data")          ## NULL then it has already been
    return(ai)                              ## computed so just return it.
  }
  data <- cm$get()                          ## Otherwise solve for the inverse
  ai <- solve(data, ...)                    ## and put it into the cm object
  cm$setsolve(ai)                           ## using its setsolve method,
  ai                                        ## and return the inverse, ai.
}
