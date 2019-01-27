## Together, these functions cache the potentially time-consuming computation of calculating the
## inverse of a square matrix. Therefore, if the inverse of the matrix has already been calculated,
## it returns the transformed matrix instead of re-calculating the inverse.

# This function takes in a square matrix and creates a list containing functions to:
# 1) set the value of the matrix, 2) get the value of the matrix, 3) set the value of the matrix, and
# 4) get the value of the matrix. This allows computations that have already been caculated to
# be cached, thus reducing computational time.

makeCacheMatrix = function(mat = matrix()) {
  if (!is.matrix(mat) || ncol(mat) != nrow(mat)) {
    message("Please enter a square matrix")
  }
  inv = NULL
  set = function(y) {
    mat <<- y
    inv <<- NULL
  }
  get = function() mat
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function first checks to see if the inverse of the matrix has already been calculated
# and stored in the makeCacheMatrix function. If it has, then the value is returned.
# Otherwise, the inverse of the square matrix is calculated, returned, and cached for future use.

cacheSolve = function(mat, ...) {
  inv = mat$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data = mat$get()
  inv = solve(data, ...)
  mat$setinv(inv)
  inv
}
