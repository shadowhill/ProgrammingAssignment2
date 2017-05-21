## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## 1st function gose to here:
## give an objective matix.(random a n*n square matrix)
## sample: x <- makeCacheMatrix(5) will generate a 5*5 square matrix.
makeCacheMatrix <- function(n) {
  x<-matrix(rnorm(n*n),n,n)
  im <- NULL
  set <-function(y,n) {
    x <<- matrix(y,n,n)
   im <<- NULL
  }
  get<-function() x
  setinvm <-function(inversemartix) im <<-inversemartix
  getinvm<-function() im
  
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}

## 2nd function gose to here:
## Inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <-x$getinvm()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m<-solve(data,...)
  x$setinvm(m)
  m
  
}

## check matrix Functions:
## A^-1 %*% A =I
## checkMF<-function(x,y){
##   round(x$get()%*%y,2)
## }
