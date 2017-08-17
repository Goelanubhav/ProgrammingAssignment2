## Function to set and retrieve the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## function to set the matrix values
  set <- function(y) {
    print(class(y))
    x <<- y
    inv <<- NULL
  }
  ## function to get the matrix values
  get <- function() x
  ## Function to set the inverse matrix
  setinv <- function(inverse) inv <<- inverse
  ## Function to get the inverse matrix
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## Function to validate if inverse matrix data is derived from cache or calculated

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## try to get the cached data
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

## If cache data not found calculate the inverse of matrix and return
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