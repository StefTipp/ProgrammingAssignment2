## makeCacheMatrix receives a matrix and returns a list containing four functions that
## set the value of the matrix, get the value of the matrix, set the inverse of the matrix and 
## get the inverse of the matrix

## makeCacheMatrix receives an input matrix x, which will then be saved in the cache; the output list contains four 
## different functions which will be called when cacheSolve is running

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL  # sets the value of the inverse, m, to NULL (placeholder; provides a default if cacheSolve has not yet been used)
  
  set<-function(y){
    x<<-y  # caches the input matrix x; thereby cacheSolve can check if it has changed or not
    m<<-NULL  # sets the inverse to NULL (provides default)
  }
  get<-function() x  # returns the matrix x
  setmatrix<-function(inv) m<<- inv # saves the inverse to m in the enclosing environment / caches it; m is NULL the first time
  getmatrix<-function() m  # returns the inverse of the matrix; m is NULL the first time this is called
  list(set=set, get=get,  # creates a list containing all the four functions set, get, setmatrix and getmatrix 
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}



## cacheSolve checks if the inverse of the input matrix x has been calculated already, if not it will do so

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()  # retrieves the inverse matrix m from the cache
  if(!is.null(m)){  # if m is not NULL, the inverse matrix is obtained from the cache
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()  # if m is NULL, the matrix created is obtained and the inverse m is calculate
  m<-solve(matrix, ...) # calculates the inverse and stores it in m
  x$setmatrix(m)  # 
  m

}
