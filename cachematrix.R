## Put comments here that give an overall description of what your
## functions do

## The first function is creating a list with with function solve and matrix to be inversed
## The second function check if the inverse of the matrix already exist 

## Write a short comment describing this function
       
##The first function, `makeCacheMatrix` creates a special "vector", which is
##really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

                m <- NULL
                set <- function(y) {
                                x <<- y
                                m <<- NULL
                        }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
                list(set = set, get = get,
                    setsolve = setsolve,
                    getsolve = getsolve )
                }



## Write a short comment describing this function

## The following function calculates the inverse of the special "vector"
## created with the above function. However, it first checks to see if the
## inversse of the matrix has already been calculated. If so, it `get`s the inverse of the matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setsolve`
## function.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        ## Check if matrix is in cache by previously using makeCacheMatrix function
        ## by checking if m is null 
        ## if not return m
        m <- x$getsolve()
                if(!is.null(m)) {
                        message("getting cached matrix")
                        return(m)
                }
        ## if m is null calculate the inverse of the matrix with solve function
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
                m 
}
