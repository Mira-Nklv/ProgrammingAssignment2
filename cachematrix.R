## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_mat <<- inverse
        getInverse <- function() inv_mat
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## This function prints the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inv_materse of 'x'
        
        inv_mat <- x$getInverse()
        if (!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        mat <- x$get()
        inv_mat <- solve(mat, ...)
        x$setInverse(inv_mat)
        inv_mat
}

## Testing functions

B <- matrix(1:4,2)
B
B1 <- makeCacheMatrix(B)
cacheSolve(B1)

