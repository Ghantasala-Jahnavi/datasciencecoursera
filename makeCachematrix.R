makeCacheMatrix <- function(x = matrix()) {
  b <- NULL
  set <- function(y){
    x <<- y
    b <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {b <<- inverse}
  getInverse <- function() {b} 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  b<- x$getInverse()
  if(!is.null(b)){
    message("getting cached data")
    return(b)
  }
  mat <- x$get()
  b <- solve(mat,...)
  x$setInverse(b)
  b
}
