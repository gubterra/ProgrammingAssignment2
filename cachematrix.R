
makeCacheMatrix <- function( mat = matrix() ) {
  
  ## Init var
    inv <- NULL
  
  set <- function( matrix ) {
  mat <<- matrix
  inv <<- NULL
    }

  get <- function() {
    mat
  }
  
  ##  set inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## get inverse of the matrix
  getInverse <- function() {
    ## Return inv 
    inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  ## Return a matrix inverse of 'x'
  mat <- x$getInverse()
  
  ## Return the inverse if its set
  if( !is.null(mat) ) {
    message("caching...")
    return(mat)
  }
     
  data <- x$get()
    
  mat <- solve(data) %*% data
    
  x$setInverse(mat)
  
  ## Return matrix
  mat
}