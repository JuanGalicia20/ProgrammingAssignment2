##Juan Galicia
## Functions that calculates the inverse of a provided matrix

## Write a short comment describing this function
## Function that creates a special matrix object and states the list of child fuctions

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y)
  {
    s<<-y
    m<<-NULL
  }
  
  ##child functions that are going to be used as setters and getters of values
  get<-function() x
  setinv <-function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##Function that calculates the inverse of the matrix, using the "solve" fuction
##because if we dont declare the 'b' argument in solve() is taken to be an identity matrix and 
##will return the thing that we wanted that is the inverse of the matrix provided. All of these
##if it hadnt been calculated previously, if it does it skips the computation as in the example.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  
  ##check if its already calculated
  if(!is.null(m))
  {
    message("getting data")
    return(m)
  }
  
  ##does the computation and returns the value
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m
}
