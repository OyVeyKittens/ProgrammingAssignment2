## This is a matrix that can cache its own inverse.
 
## This is a function to make the cache of the inverse (solve)
 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
 
 ## This is a function that calls the cache if it exists, else it calculates the inverse
 
cacheSolve <- function(x, ...) {
    i <- x[getsolve()]
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data <- x[get()]
    i <- solve(data, ...)
    x[setsolve(i)]
    i
     ## Return a matrix that is the inverse of 'x'
 }