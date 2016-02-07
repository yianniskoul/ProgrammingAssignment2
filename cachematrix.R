#Matrix inversion is usually a costly computation and there may be some benefit to caching 
#the inverse of a matrix rather than compute it repeatedly.
#These two functions cache the inverse of a matrix.



makeCacheMatrix <- function(x = matrix()) { #This function creates a special "matrix" object that can cache its inverse.
  inv <- NULL
  set <- function(y) { #setting the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x #getting the value of the matrix
  setinverse <- function(inverse) inv <<- inverse #setting the value of inverse of the matrix
  getinverse <- function() inv #getting the value of inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #creating a list with all the above values
}


cacheSolve <- function(x, ...) { #This function computes the inverse of the special "matrix" returned by makeCacheMatrix
  inv <- x$getinverse()
  if(!is.null(inv)) { #if the inverse has already been computed
    message("fetching data from cache")
    return(inv) #it is fetching the inverse from cache
  }
  data <- x$get()
  inv <- solve(data) #else it computes the inverse
  x$setinverse(inv)
  inv #returns the inverse
}

#k = rbind(c(1, 3), c(3, 1))
#y = makeCacheMatrix(k)
#y$get()
#cacheSolve(y)
#cacheSolve(y)