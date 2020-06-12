## Matrix inversion is usually a costly computation and there may be 
##some benefit to caching the inverse of a matrix rather than compute 
##it repeatedly (there are also alternatives to matrix inversion that 
##we will not discuss here).My assignment shows a pair of 
##functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix() ) ##initialized x within 
  {
  m <- NULL                                ##initialized m
  
  set <- function(y)                       ##set function defined with argument y
  {
    x <<- y                                ##assigning value 'y' to object 'x' in parent environment
    
    m <<- NULL                             ##Assign the value of NULL to the m object in the parent
                                           ##environment. This line of code clears any value of m that
                                           ##had been cached by a prior execution of cachemean()
  }
  get <- function()x                       ##Since the symbol x is not defined within get(),R retrieves 
                                           ##it from the parent environment of makeCacheMatrix()
  
  setinverse <- function(inverse) m <<- inverse ##setinverse defined
  
  getinverse <- function() m                    ##getinverse defined.the code uses the <<- form of the 
                                                ##assignment operator to assign the input argument to the
                                                ##value of m in the parent environment.
  
  
  list(set = set, get = get,               ##Creatting new object by returning list()
       setinverse = setinverse,            ##each element in the list is created with 
                                           ##each elementName = value syntax ex.set = set
       getinverse = getinverse)
  

}


## makeCacheMatrix:  This function creates a special "matrix" object that 
## can cache its inverse.


cacheSolve <- function(x, ...)             ##Defined cachesolve with argument x
  {
                                           ## Return a matrix that is the inverse of 'x'
 
   m <- x$getinverse()                     ##attempts to retrieve a inverse from the  
                                           ##object passed in as the argument
  
   
   if(!is.null(m))                         ##To see whether the result is NULL.
                                          
  {
    message("getting cached data")         ##Since makeCachedMatrix() sets the cached inverse matrix
                                           ## to NULL whenever a new vector is set into the object, 
                                           ##if the value here is not equal to NULL, we have a valid
                                           ## cached inverse and can return it to the parent environment 
    return(m)
  }
  data <- x$get()                          ##If FALSE then inverse is calculated and returned
  m <- solve(data, ...)
  x$setinverse(m)
  m
         
       
}
