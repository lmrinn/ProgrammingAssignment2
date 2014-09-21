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
  
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse) #previous functions are collected in a list and returned to the calling environment
  
}


## This function is called when the user wants to access the inverse
## Firstly, the value stored in the matrix object for the inverse is obtained
## If it is not NULL, the inverse has already been computed and will be returned
## If it is NULL, the inverse will be computed and stored in the matrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse() #the value cached for the inverse is obtained
  
  if(!is.null(inv)){
    #if the inverse has already been computed (not NULL) it is returned
    message("getting cached inverse")
    return(inv) #return the cached inverse
  }
  
  #the inverse has not been computed
  data <- x$get() #the matrix' data is obtained
  inv <- solve(data, ...) #the inverse is computed
  x$setInverse(inv) #the computed inverse is stored in the special matrix object
  inv #return the computed inverse
}
