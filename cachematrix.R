## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) 
{
     m <- NULL
  set <- function(y)
     {  x <<- y
        m <<- NULL 
     }
     
  get <- function() {x}
    
  setinverse <- function(mean) {m <<- mean}
     
  getinverse <- function() { m }
     
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}    



## Computes the inverse after checking if the cache already exists 

cacheSolve <- function(x, ...)
{   m <- x$getinverse() 
    if(!is.null(m))
    { message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
        
  
  
  
  
  
  
  