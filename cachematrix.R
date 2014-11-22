## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat <- x$getInv()
    if(!is.null(mat)){
      return(mat)
    }
    disp_mat <- x$get()
    inv <- solve(disp_mat)    
    x$setInv(inv)
    inv
}
