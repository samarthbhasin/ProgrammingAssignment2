## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
##     that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
##     "matrix" returned by `makeCacheMatrix` above. If the inverse has
##     already been calculated (and the matrix has not changed), then
##     `cacheSolve` should retrieve the inverse from the cache.


## generate a 'special' matrix to be used by function cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
         set <- function(y){
                x <<- y
                inv <<- NULL
         }
         get <- function() x
         setinv <- function(solve) inv<<-solve
         getinv <- function() inv
         list(set = set, get = get,
              setinv = setinv, getinv = getinv)
}


## Inverse of a Matrix: Calculate or get cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data for inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
