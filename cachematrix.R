# The makeCacheMatrix function will create a special matrix object that can cache its inverse
# The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # this is where the result of inversion is stored 
  set <- function(y) { #set the value of the matrix
    x <<- y # caches the inputted matrix
    m <<- NULL # sets the value of m (the matrix inverse) to null
  }
  get <- function() x # return the input matrix
  setInv <- function(inv) m <<- inv # set the inversed matrix
  getInv <- function() m # return inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inversed matrix from object x
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() #get the matrix object
  m <- solve(data, ...) #calculate the inverse
  x$setInv(m) #set the inverse
  m # return the solved inverse
}

#You can test this function by generating a matrix, called test
test <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
#Load "test" matrix into your function to make a new object called test2
test2 <- makeCacheMatrix(test)
#Run cachesolve on test2 to see if it calculates the inverse of test matrix
cacheSolve(test2)