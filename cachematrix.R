## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: sets a matrix, gets a matrix, sets the inverse of matrix, gets the inverse of matrix
## cacheSolve: retrieves from cache the inverse of the matrix, or sets the inverse if not in cache
## usage: source ("cachematrix.R")

## Write a short comment describing this function
## input: a matrix
## set matrix, get matrix, setinverse of matrix, getinverse of matrix
## initialize & set matrix: x <- makeCacheMatrix(matrix(c(5,6,7,8),nrow=2,ncol=2))
## set new matrix value:  x$set(matrix(c(2,3,4,5),nrow=2,ncol=2))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     # set current matrix value to null
  set <- function(y) {  # set new matrix value
    x <<- y
    m <<- NULL
  }
  get <- function() x  # return current matrix
  setinverse <- function(solve) m <<- solve  # set inverse of matrix
  getinverse <- function() m  # return current inverse of matrix
  list(set = set, get = get,  # define list of exposed functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## input: instance of makeCacheMatrix
## output: inverse of matrix from cache, or recompute & cache inverse in makeCacheMatrix and return that inverse
## usage: get inverse of matrix: cacheSolve(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()  # get current matrix inverse value
  if(!is.null(m)) {    # if inverse has already been set then return inversed matrix
    message("getting cached inverse data")
    return(m)
  }
  data <- x$get()        # get current matrix (not inversed)
  m <- solve(data, ...)  # inverse matrix
  x$setinverse(m)        # set the inverse of the matrix 
  m # return the matrix inverse
}
