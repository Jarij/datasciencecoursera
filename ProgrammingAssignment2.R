# Write the following functions:
#   
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

# Examples given
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }

# The following function calculates the mean of the special "vector" created with the above function. 
# However, it first checks to see if the mean has already been calculated. If so, it gets the mean from 
# the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value 
# of the mean in the cache via the setmean function.
# This function assumes that the matrix is always invertible.

# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL            # defaul for m in case cacheSolve not yet been used  
  set <- function(y)   
    {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    set_matrix <- function(inverse) m <<- inverse
    get_matrix <- function() m
    list(set=set, get=get, set_matrix=set_matrix, get_matrix=get_matrix)
}

cacheSolve <- function(x=matrix(), ...) 
{
  m <- x$get_matrix()
  if(!is.null(m))      # Check if cacheSolve has been executed before
  {
    message("getting cached data.")
    return(m)
  }
  m <- solve(x$get())
  x$set_matrix(m)
  m
}
