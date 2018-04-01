## This document comprises two functions, makeCacheMatrix and cacheSolve functions.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse, while 
## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix. 
## cacheSolve retrieve the inverse of the matrix from the cache if it has already been calculated


## the following function assumes the matrix argument is invertible
## And creates a special vector which is a list containing a function that:
## 1. set the value of the matrix, 2. get the value of the matrix
## 3. set the value of the inverse, 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function () x
	set_inverse <- function(solve) i <<- solve
	get_inverse <- function() i
	list (set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix
## if inverse has calculated, cacheSolve return the inverse calculated. Else it calculates the inverse of the matix
## and sets the value of the inverse in the cache via the set_inverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$get_inverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$set_inverse(i)
	i
}
