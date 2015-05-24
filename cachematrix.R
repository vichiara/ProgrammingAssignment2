#The function makeCacheMatrix creates a special "matrix", which containts a function that 
#       set the value of the matrix,
#       get the value of the matrix, 
#       set the value of the inverse
#       get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv_matrix<- function(solve) inv <<- solve
        getinv_matrix <- function() inv
        list(set = set, get = get,
             setinv_matrix = setinv_matrix,
             getinv_matrix = getinv_matrix)
}

## the function cacheSolve calculates the inverse of the special "matrix" created with the function makeCacheMatrix. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse_matrix function.


cacheSolve <- function(x, ...) {
       inv <- x$getinv_matrix()
       if(!is.null(inverse)) {
               message("getting cached data")
               return(inv)
       }
        data <- x$get()
       inv <- solve(data, ...)
       x$setinv_matrix(inv)
       inv
}

