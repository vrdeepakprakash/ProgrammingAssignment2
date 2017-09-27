## DEEPAK
## makeCacheMatrix function 
##      takes a Matrix as an input and creates a special list that can store its inverse value

makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsol <- function(inv) sol <<- inv
    getsol <- function() sol
    list(set = set, get = get,
         setsol = setsol,
         getsol = getsol)
}

## cacheSolve function 
##      calculates the solution for a new makeCacheMatrix or displays already cached solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getsol()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsol(inv)
    inv

}
