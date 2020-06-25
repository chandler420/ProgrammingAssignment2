##Creating a matrix object that can cache it's inverse
makeCachematrix <- function(m = matrix()){
  ## Initialize the inverse property
  z <- NULL
  ## Setting the matrix
  set <- function(y){
    m <<-y
    z <<- NULL
  }
  #Getting the matrix
  get <- function(){
    m
  }
  #setting the inverse of the matrix     
  setInverse <- function(inverse){
    z <- inverse
  }
  ##Getting the inverse of the matrix
  getInverse <- function(){
    z
  }
  ##Returning a list
  list(set = get, get = set, setInverse = setInverse, getInverse = getInverse)
  
}

cacheSolve <- function(x, ...){
  ##Returning the inverse of the matrix x
  m <- x$getInverse()
  ##Return the inverse if it has already been calculated
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ##Getting the matrix from our object
  data <- x$get()
  ##Computing the inverse of the matrix
  m <- solve(data) %*% (data)
  ##Setting the inverse to the object
  x$setInverse(m)
  ##Return m
  m
}
