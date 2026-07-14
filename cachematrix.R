## functions are creating a object(special matrix), can store a matrix
## and caching matrix's inverse. The matrix's inverse is calculated only once and reused
## untill the matrix will changed.

## Creating a special matrix object, storing the matrix and caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y  ##assigning in parent env and remove last cache
    m <<- NULL
  }
  get <- function() x   ### current matrix x, return the.
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(get = get , set = set,   ##return all the function associated to object
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cachesolve is Calculating the matrix inverse.
## If the inverse has already been calculated, it returns the cached value.
#rather than calculating it again.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()  #checking whether the inverse is store or not in cache
  if(!is.null(m)){   ##if inverse present than return cached.
    message("getting cached data")
    return(m)   
  }
  data <- x$get()
  m <- solve(data, ...) #solve() used to get inverse of matrix
  x$setinverse(m)   ##store cached inverse
  m ## Return a matrix inverse, which is calculated
}
