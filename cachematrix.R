## The two functions interact with each other. The aim is to
## allow the user to avoid repeating the potentially costly
## computation of the inverse of a matrix when the result
## has been computed earlier.
##
## 'makeCashMatrix' creates an object containing four functions:
## set      stores the original matrix x
## get      returns the matrix x
## setinv   assigns the inverse of x to an internal variable
## getinv   returns the inverse of x
## setinv and getinv are normally not accessed by the user
## directly. These functions are used by 'cacheSolve'.
##
## Here's an example how to use these functions:
## x <- matrix(rnorm(40*40,40,40)   # a random 40x40 matrix, hopefully not singular
## o <- makeCacheMatrix(x)          # create object o containing four functions
## cacheSolve(o)                    # compute inverse of x
## cacheSolve(o)                    # return cached inverse of x, without re-computing it
## x <- matrix(rnorm(40*40,40,40)   # a random 40x40 matrix, hopefully not singular
## o$set(x)                         # set new matrix x (and lose cached inverse in the process)
## cacheSolve(o)                    # compute inverse of the new x
## cacheSolve(o)                    # return cached inverse of x, without re-computing it

## create object containingh four functions: set (sets a matrix), get (return this matrix),
## setinv (stores inverse of x in cache), getinv (returned cached inverse)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Returns the inverse of the matrix stored in an object that was created with
## makeCacheMatrix. If the inverse is not in cashe, it is computed and stored in
# cache for later use.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    inverse <- x$get()
    inverse <- solve(inverse)
    x$setinv(inverse)
    inverse
}
