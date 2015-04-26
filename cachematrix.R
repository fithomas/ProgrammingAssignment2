## The function makeCacheMatrix creates an object that can be used to execute the cacheSolve function

## The following function creates a  list that contains functions to set and get the matrix 
## as well as to set and get the inverse matrix. The method set that sets the matrix changes only the matrix,
## if there is really a change otherwise it returns a warning. This should help to avoid double computations,
## if the special matrix object already contains this matrix.

makeCacheMatrix <- function(x = matrix()) {
  if(class(x) != "matrix") {
    stop("This function needs as an input a matrix type object!")
  }
  cim<-NULL # variable for the cached inverse matrix which is initially NULL
  
  # function to return the matrix of the special matrix object
  get<-function(){
    x
  }
  
  # function to change the matrix of the special matrix object
  set <- function(y) {
    # change it only if the matrix really has changed
    if(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)){
      # the matrix is equal to the existing one, don't change anything
      warning("this will not make any changes to the existing matrix, because you have submitted the same matrix")
    }else{  
      # the matrix is different
      x <<- y # change it
      cim <<- NULL # the cached inverse matrix should be set to null, because it has to be computed again
    }
  }
  
  
  # function to get the cached inverse matrix from the cache
  getInverseMatrix <- function(){
    cim
  }
  
  # function to set the cached inverse matrix
  setInverseMatrix<- function(y){
    cim<<- y
  }
    
  # return the list
  list(type="CacheMatrix",get=get, set=set, setInverseMatrix=setInverseMatrix, getInverseMatrix = getInverseMatrix)  
}


## This functions gets as an input a special matrix object (this is checked before any computations).
## If there is already an inverse matrix, the function returns this one. Otherwise it gets the underlying matrix
## from the special matrix object, computes the inverse matrix, stores the inverse matrix in the cache and returns 
## the solution to the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (!class(x)=="list" || !"type" %in% attributes(x)$names || x$type != "CacheMatrix"){
    stop("This is not an object of type CacheMatrix. Create one with the function makeCacheMatrix!")  
  }
  
  im<-x$getInverseMatrix()
  if(!is.null(im)){
    
    print("returning the cached inverse matrix")
    
  }else{
    # there is no cached inverse matrix
    print("no cached matrix")
    
    #calculate the inverse matrix
    print("compute the inverse matrix")
    im<-solve(x$get())
    
    # store this matrix in the cache for later computations
    x$setInverseMatrix(im)
  }
  
  # return the inverse matrix
  im
}

# ****************************************************************************************************************
# Some Tests
# ****************************************************************************************************************

# ****************************************************************************************************************
# Test 1 - solve it the standard way
# ****************************************************************************************************************

m = matrix(c(1, 0.2, 0.5, 0.3, 1, 0.5, 0.3, 0.2, 1), nrow=3,ncol=3) 

# standard way
solve(m)

# > solve(m)
#            [,1]       [,2]       [,3]
# [1,]  1.2000000 -0.2000000 -0.3200000
# [2,] -0.1333333  1.1333333 -0.1866667
# [3,] -0.5333333 -0.4666667  1.2533333

# ****************************************************************************************************************
# Test 2 - create a CachedMatrix and solve it multiple times
# ****************************************************************************************************************

# create a cached matrix object
cm<-makeCacheMatrix(m)


# get inverse matrix -> needs to be computed first
cacheSolve(cm)

# > cacheSolve(cm)
# [1] "no cached matrix"
# [1] "compute the inverse matrix"
#            [,1]       [,2]       [,3]
# [1,]  1.2000000 -0.2000000 -0.3200000
# [2,] -0.1333333  1.1333333 -0.1866667
# [3,] -0.5333333 -0.4666667  1.2533333
  

# get the inverse matrix again --> return it from the cache
cacheSolve(cm)

# > cacheSolve(cm)
# [1] "returning the cached inverse matrix"
# [,1]       [,2]       [,3]
# [1,]  1.2000000 -0.2000000 -0.3200000
# [2,] -0.1333333  1.1333333 -0.1866667
# [3,] -0.5333333 -0.4666667  1.2533333

# ****************************************************************************************************************
# Test 3 - change the initial matrix and solve it 
# ****************************************************************************************************************


# Change the matrix 
m = matrix(c(1, 0.3, 0.5, 0.3, 1, 0.5, 0.3, 0.2, 1), nrow=3,ncol=3) 
cm$set(m)

# get the inverse matrix --> needs a recomputation of the inverse matrix
cacheSolve(cm)

# > cacheSolve(cm)
# [1] "no cached matrix"
# [1] "compute the inverse matrix"
# [,1]       [,2]       [,3]
# [1,]  1.2244898 -0.2040816 -0.3265306
# [2,] -0.2721088  1.1564626 -0.1496599
# [3,] -0.4761905 -0.4761905  1.2380952

# ****************************************************************************************************************
# Test 4 - set the matrix to the same matrix
# ****************************************************************************************************************

# Set the matrix again without any changes to its values 
# --> should provide a warning message to the user, because no changes will be made
cm$set(m)

# > cm$set(m)
# Warning message:
#   In cm$set(m) :
#   this will not make any changes to the existing matrix, because you have submitted the same matrix


# should return the inverse matrix
cacheSolve(cm)

# > cacheSolve(cm)
# [1] "returning the cached inverse matrix"
# [,1]       [,2]       [,3]
# [1,]  1.2244898 -0.2040816 -0.3265306
# [2,] -0.2721088  1.1564626 -0.1496599
# [3,] -0.4761905 -0.4761905  1.2380952

# ****************************************************************************************************************
# Test 5 - test the make function with wrong inputs
# ****************************************************************************************************************

makeCacheMatrix(c(5))

# Error in makeCacheMatrix(c(5)) : 
#   This function needs as an input a matrix type object!

# ****************************************************************************************************************
# Test 6 - test the functions with wrong inputs
# ****************************************************************************************************************

# Test the function with a wrong input (normal matrix) --> should return an error message
cacheSolve(m)

# > cacheSolve(m)
# Error in cacheSolve(m) : 
#   This is not an object of type CacheMatrix. Create one with the function makeCacheMatrix!


# Test the function with a wrong input (some list) --> should return an error message
l <- list(a=c(1:5))

cacheSolve(l)

# > cacheSolve(l)
# Error in cacheSolve(l) : 
#   This is not an object of type CacheMatrix. Create one with the function makeCacheMatrix!

# Test the function with a wrong input (some list with attribute type) --> should return an error message
l <- list(type=c(1:5))

cacheSolve(l)

# > cacheSolve(l)
# Error in cacheSolve(l) : 
#   This is not an object of type CacheMatrix. Create one with the function makeCacheMatrix!

