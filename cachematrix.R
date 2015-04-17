## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This method creates a list containing the matrix and functions to calculate the inverse
## and to cache it for future use
makeCacheMatrix <- function(x = matrix()) {

        m.inverse <- NULL
        set <- function(y){
                x <<- y
                m.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m.inverse <<- inverse
        getinverse <- function() m.inverse
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Write a short comment describing this function

## This method return the inverse of a square matrix using the list created from
## the makeCacheMatrix method.  If first checks to see if the inverse has been
## cached then either returns the inverse or solves for it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        mat2 <- x$get()
        m.inv <- solve(mat2)
        x$setinverse(m.inv)
        m.inv
        
}

## Starter method for convinence
runAssignment2 <- function(x,dims=2){
        
        mat = matrix (x,nrow=dims,ncol=dims)
        mat1 = makeCacheMatrix(mat)
        cacheSolve(mat1)
}
