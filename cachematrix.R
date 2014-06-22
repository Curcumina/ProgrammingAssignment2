## Assignment2: Caching the Inverse of a Matrix
## first function makeCacheMatrix creates a special "matrix" 

## functions to set and get the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
}
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

## computes the inverse of makeCacheMatrix
## cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    retrun(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
## Setting up matrix for test 
##matrix(1:4, 2, 2)
> a$set(matrix(1:4, 2, 2))
> a$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
## retriving matrix by cacheSolve(a)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

