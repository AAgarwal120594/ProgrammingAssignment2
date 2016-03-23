# it will set value and assign to cheche memory


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #this is where resilt of inversion is stored
  # A setter function, use this to set a matrix to object created by makeCacheMatrix function
  set <- function(y) {
    x <<- y
    m <<- NULL # it also initialises m to null
  }
  get <- function() x # return the input matrix
  setinv <- function(inv) m <<- inv # set the inversed matrix
  getinv <- function() m # return the inversed matrix
  # return a list that contains these functions, so that we can use
  # makeCacheMatrix object like these
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
# it will get value from cache memory if its available otherwise it will calculate
cacheSolve <- function(x, ...) {
  m <- x$getinv() # get the inversed matrix from object x
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data, ...) # we solve it
  x$setinv(m) # we then set it to the object
  m # return the solved result
}
#TEST
#test<- matrix(runif(9,1,100),3,3)
#testca<- makeCacheMatrix(test)
#testmean <- cacheSolve(testca)
#testmean
