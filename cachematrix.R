## Function to set and retrieve the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    print(class(y))
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## Function to validate if data can be derived from cache or to be calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv

}


##x = rbind(c(1, 2), c(2, 1))
##inv = makeCacheMatrix(x)
##inv$get()
##cacheSolve(inv)
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333


##git clone git@github.com:goelanubhav7/ProgrammingAssignment2.git /c/SCm/coursera/DST-1/ProgrammingAssignment3-master
##git remote add upstream https://github.com/goelanubhav7/ProgrammingAssignment2.git
