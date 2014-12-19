## GENERAL DESCRIPTION 
## The function makeCacheMatrix receives an invertible matrix (x) and creates a special 
## matrix that can cache its inverse (im), and the function cacheSolve receives a 
## special matrix (created by the makeCacheMatrix function), calculates its inverse, 
## but if the inverse have already been calculated, cacheSolve can retreive the
## inverse from the cache.

## FUNCTION 1: makeCacheMatrix
## makeCacheMatrix receives an invertible matrix called x and creates a special matrix,
## doing the following: 1. Set the value of the matrix and put the inverse matrix (im) 
## in NULL (so if x data changes, im has to be recalculated), 2. Get the value or data 
## of the matrix, 3. Set the value of the inverse matrix, 4. Get the value of the
## inverse matrix. 5. Return the special matrix.

makeCacheMatrix <- function(x = matrix()) {
        ##im: Inverse matrix, x: Invertible matrix
        im <- NULL
        
        set <- function(y) {
                x <<- y
                im <<- NULL                
        }
       
        get <- function() x
        
        setim <- function(inversem) im <<- inversem
        
        getim <- function() im
        
        list(set = set, get = get,
             setim = setim,
             getim = getim)
}


## FUNCTION 2: cacheSolve
## cacheSolve receives a special matrix (created by FUNCTION 1) and calculates or
## retreives its inverse doing the following: 1. Get the inverse matrix im, 2. Return
## the cached inverse matrix if im is not NULL and prints a message indicating the
## action, 3. Calculate (using the solve R function) the inverse matrix, store (cache) 
## the new im and return it.

cacheSolve <- function(x, ...) {
        ##im: Inverse matrix, x: special matrix
        
        im <- x$getim()
        
        if(!is.null(im)) {
                message("Getting cached inverse matrix")
                return(im)
        }
        
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
              
        im
}
