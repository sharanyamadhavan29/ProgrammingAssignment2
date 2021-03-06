## The funtion creates a matrix, checks if the cache exists, returns the cache value if yes and computes otherwise 

## Creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) 
{
     m <- NULL
  set <- function(y)
     {  x <<- y
        m <<- NULL 
     }
     
  get <- function() {x}
    
  setinverse <- function(inverse) {m <<- inverse}
     
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
        
  
  
  
  
  
  
  