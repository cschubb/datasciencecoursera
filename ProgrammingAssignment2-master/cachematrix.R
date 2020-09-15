makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}



a1 <- c(3, 2, 5) 
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4) 



A <- rbind(a1, a2, a3) 




my_vector <- makeCacheMatrix(A)
my_vector$getInverse() 
my_vector$get()     
cacheSolve(my_vector) 



#make up some function of matrix A
B=A+10*log(2)
my_vector$setInverse(B) 
my_vector$getInverse() 
my_vector <- makeCacheMatrix(B)
cacheSolve(my_vector) 