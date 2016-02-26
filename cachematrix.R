## makeCacheMatrix is a function that can create a matrix that can cache its inverse. cacheSolve solves for the inverse of the matrix made in makeCacheMatrix or simply retreives it if it has already been computed. 

## makeCacheMatrix takes the argument of a matrix. Set sets the value of the matrix that is inputted in the function and get retrieves the set value of the matrix put into the original function. Setinverse sets the inverse value of the matrix and getinverse gets the invserse value of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve solves for the inverse of the matrix that is chached in makeCacheMatrix. If it was already computed then it will retreive the inverse matrix from the chache.   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
