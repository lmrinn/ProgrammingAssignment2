## To minimize computing time intensive matrix inversions, these two functions
## cache the inverse after it has been computed. The next time, the inverse is needed
## only the cached inverse is accessed. If the original matrix is changed, the
## previously cached inverse is set to NULL, since it has to be recomputed. This
## will happen the next time the inverse is accessed



## This function creates a special matrix object that contains the original matrix
## and can store its inverse. Furthermore it provides the functions to set and get
## the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #the inverse variable is initialized with NULL
  
  set <- function(y){
    #function to update the matrix. After updating, the old inverse becomes invalid,
    #hence, it is set to NULL (which will require recomputing the next time it is accessed)
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x #return function of the stored matrix
  
  setInverse <- function(inverse) inv <<- inverse #computed inverse is stored in the object
  
  getInverse <- function() inv #return function for the inverse
  
  list(set = set, get = get
       setInverse = setInverse, getInverse = getInverse) #previous functions are collected in a list and returned to the calling environment
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
