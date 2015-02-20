## This file contains
## - makeCacheMatrix(m): creating matrix function
## - cacheSolve(m): solving and chaching function
## - test(): testing function, you can run test() from complete check


## makeCacheMatrix(matrix)
## This function creates a special "matrix" object that can
## cache its inverse.
## Function implement:
## - get() method
## - set() method
## - getsolve() method
## - setsolve() method
makeCacheMatrix <- function(m = matrix()) {

  inversedMatrix <- NULL # clear old values
  
  ## set(matrix)
  ## Sets original matrix and clear the cached value
  set <- function(y) 
  {
    ## set original matrix
    m <<- y
    
    ## clear the cached value
    inversedMatrix <<- NULL
  } 
  
  ## get()
  ## return original matrix
  get <- function()
  {
    m
  }   

  ## setinverse(matrix)
  ## sets (not compute!!!) the value of the inverse of the original matrix
  setinverse <- function(invMatrix) 
  {
    inversedMatrix <<- invMatrix
  } 
  
  ## Return (not compute!!!) the cached value of the inverse of the matrix.
  ## Returns NULL if the cached value not computed and not set or if the
  ## original matrix has changed
  getinverse <- function() 
  {
    inversedMatrix
  } 
  
  ## return a list of methods
  list(
      set = set, 
      get = get,
      setinverse = setinverse,
      getinverse = getinverse
  )   
}


## cacheSolve(matrix, ...)
## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(m, ...) {
  ## check the inverse of the originam matrix computed or not
  invMatrix <- m$getinverse()
  if(!is.null(invMatrix)) 
  {
    ## The inverse of the originam matrix already computed
    message("getting cached data ... ", appendLF = FALSE)
    Sys.sleep(1)
    message("done")
    
    return(invMatrix)
  } 
  
  ## The inverse of the originam matrix not computed
  message("computing inversed matrix ... ", appendLF = FALSE)
  
  ## get original matrix
  data <- m$get()
  
  ## Compute the inverse 
  invMatrix <- solve(data, ...)
  
  ## Set computed inversed value into base matrix
  m$setinverse(invMatrix)
  
  Sys.sleep(1)
  message("done")
  
  ## Inversed value
  invMatrix
}


## Function for testing makeCacheMatrix(m) and cacheSolve(m)
test <- function() {
  message("Let's try :)")
  message("")
  
  ## Step 1: Create original matrix
  
  ## Create cache-matrix
  message("Let's create matrix 2x2")
  matrixObject = makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
  message("matrixObject created")
  
  ## Show original matrix
  print(matrixObject$get())
  message("")
  
  ## Check inversed matrix
  message("Let's check inversed value")
  if (is.null(matrixObject$getinverse()))
  {
    message("NULL, it is correct value")
  }
  else
  {
    message("Not NULL, something wrong")
  }
  message("")
  
  ## Compute inversed matrix
  message("Compute inversed matrix")
  cacheSolve(matrixObject)
  message("")
  
  ## Try to сompute again
  message("Compute inversed matrix again")
  cacheSolve(matrixObject)
  message("")
  
  ## check
  message("check matrix * inversed_matrix")
  print(matrixObject$get() %*% matrixObject$getinverse())
  message("")
  
  ## Step 2: Change original matrix
  
  ## Using set() method
  message("Let's change matrix")
  matrixObject$set(matrix(4:1, nrow=2, ncol=2))
  message("matrixObject changed")
  message("")
  
  ## Show changed matrix
  print(matrixObject$get())
  message("")
  
  ## Check inversed matrix
  message("Let's check inversed value of changed matrix")
  if (is.null(matrixObject$getinverse()))
  {
    message("NULL, it is correct value of changed matrix")
  }
  else
  {
    message("Not NULL, something wrong")
  }
  message("")
  
  ## Compute inversed matrix
  message("Compute inversed matrix of changed matrix")
  cacheSolve(matrixObject)
  message("")
  
  ## Try to сompute again
  message("Compute inversed matrix again")
  cacheSolve(matrixObject)
  message("")
  
  ## check
  message("check changed_matrix * changed_inversed_matrix")
  print(matrixObject$get() %*% matrixObject$getinverse())
  message("")  
}
