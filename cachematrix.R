
rm(list=ls())
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {## Here give argument x as a  "matrix"
  inv <- NULL                             ## the Inverse is initialized as Null with inv which will hold value of inverse matrix
   
  set <- function(y) {                    ## The set function is created to assign new value of matrix in parent environment if there is a new matrix
 
    x <<- y                             
    inv <<- NULL                        
}
     get <- function() x                     ## define the get fucntion - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
  ## to the functions with the $ operator
}


## The above function makeCacheMatrix returns  the the inverse of the special "matrix". 
## Where the inverse has already been calculated and exists in the cache in its original state, 
## it will be retrieved from the cache through cacheSolve function as under:-


## Write a short comment describing this function

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



Testmatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
Testmatrix$get() 
Testmatrix$getinverse()  
cacheSolve(Testmatrix)
Testmatrix$getinverse()

