## Set the value of matrix & get the value of matrix
## Set the value of inverse & get the value of inverse

library(MASS)
makeCacheMatrix <- function(x=matrix()){
      inv <- NULL                   #Initialize inverse as NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function(){x}          #Function to get matrix x
      setInverse <- function(inverse){inv <<- inverse}
      getInverse <- function(){
         inv <- ginv(x)
         inv%*%x                    #Function to obtain inverse of the matrix
      }
      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Caching the Inverse of a Matrix
## Check if the Inverse has already been calculated
## Skip the computation if calculated Inverse can be extracted from the cache

cacheSolve <- function(x, ...)      ##Gets cache data
{
      inv <- x$getInverse()
      if (!is.null(inv)) {          #Checking if inverse is NULL
              message("getting cached data")
              return(inv)           #Returns inverse value
      }
      matri <- x$get()
      inv <- solve (matri,...)      #Calculate inverse value
      x$setInverse(inv)
      inv                           ##Return a matrix that is the inverse of 'x'
}
