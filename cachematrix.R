## Put comments here that give an overall description of what your
## functions do


##This function, makeCocheMatrix, creates a list containing a function to 1. set the value of a matrix, 
##2. get the value of a matrix, 3. set the value of the inversed matrix, 
##4. get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<-NULL
        }
        get <- function() x
        set.reverse <- function(reverse) m <<- reverse
        get.reverse <- function() m
        list(set = set, get = get, set.reverse = set.reverse, get.reverse = get.reverse)

}


## This function, cacheSolve, calculate the inversed matrix created with the makeCacheMatrix function.
## It first checks if the inversed matrix has already been calculated.
## If so, get inversed matrix from the cache, Otherwise, it claculate the inverse and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get.reverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$set.reverse(m)
        m
}
