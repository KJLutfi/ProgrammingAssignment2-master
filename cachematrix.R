## From the example given, makeCacheMatrix creates a matrix which 
## contains functions that set (changes) and get (returns) the matrix and also
## set and get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {## alters the main function matrix
                x <<- y
                invert <<- NULL 
                ## returns NULL to 'invert' after each newly 
                ## created matrix
        }
        get <- function() x     
        ## returns the main function matrix
        setinvert <- function(invMa) invert <<- invMa 
        ## sets the value of the inverted matrix (if matrix is changed will be 
        ## 'NULL') and store in invert
        getinvert <- function() invert 
        ## return the value of the inverted matrix
        list(set = set, get = get, 
             setinvert = setinvert, 
             getinvert = getinvert)
}


## This function calculates the inverse of the "matrix" from the previous
## function after determining if the inverse has already been calculated.
## If not, the inverse is calculated and sets the value via "x$setinvert"

cacheSolve <- function(x, ...) {
        invert <- x$getinvert() 
        ## place the returned value of the inverted matrix into 'invert'
        if(!is.null(invert)) {
        	## if TRUE return value of inverted matrix
                message("getting inverted matrix")
                return(invert)
        }
        data <- x$get() 
        ## store matrix from makeCacheMatrix in 'data'
        invert <-solve(data, ...) 
        ## with the previous 'if' being FALSE the inverted matrix is calculated
        x$setinvert(invert) 
        ## value of inverted matrix stored in object
        invert              
        ## Return a matrix that is the inverse of 'x'
}
