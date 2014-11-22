## Two functions 
## makecacheMatrix container for 4 function get, set, setInv, getInv
## get <- will get the value that has previously been stored in the makecacheMatrix environment
## set <- will put the object into the makecacheMatrix environment
## getInv <- will get the matrix inverse
## setInv <- will inverse the matrix passed to cashSolve


makeCacheMatrix <- function(x = matrix()) {
  m_mat <- NULL
  
  get <- function(){x}
  set <- function(y) {
      x <<- y
      m_mat <<- NULL
    }
  setInv <- function(solve) {m_mat <<- solve}
  getInv <- function() {m_mat}
  list (get =get, set = set, setInv = setInv, getInv = getInv)
}

## This function checks to see if the inverse of the matrix has been stored in cache
## if it has, it returns the cached version
## if not, it inverses the matrix and then stores it to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_mat <- x$getInv()
    if(!is.null(m_mat)){
      return(m_mat)
    }
    disp_mat <- x$get()
    inv <- solve(disp_mat)    
    x$setInv(inv)
    inv
}
