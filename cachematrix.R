## R Programming Assigment 2 - Lexical Scoping ###############

#Goal: Create two functions 'makeCacheMatrix' and 'cacheSolve'
# to cache the inverse of a matrix


## 1. Create the makeCacheMatrix function, which creates a 
#special matrix object that can cache its inverse


makeCacheMatrix<- function (x = matrix ()){
  inv<- NULL
  set <- function (y) {
    x<<-y
    inv<<-NULL
  }
  
  get <- function() x #set the value of the matrix
  setInverse<- function (inverse) inv <<- inverse #set the value of the inverse
  getInverse<- function () (inv) #get the value of the inverse
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## 2. Create the cacheSolve function, which computes the inverse
## of the special matrix returned by makeCacheMatrix (if the 
## inverse has been caçculated then cachesolve should retrie-
## ve the inverse from the cache)


cacheSolve <- function (x, ...){
  inv <- x$getInverse () #returns a matrix that is tje inverse of x
  if (!is.null (inv)) {
    message ("getting cached data")
    return (inv)
  }
  mat<- x$get()
  inv<- solve (mat, ...)#compute the inverse of the matrix using the solve function
  x$setInverse(inv)
  inv
}

#testing the functions

matrix<-matrix(1:4, nrow = 2, ncol =2)
mymatrix<-makeCacheMatrix(matrix)
cacheSolve(mymatrix)



