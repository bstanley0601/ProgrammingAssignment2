#https://github.com/bstanley0601/ProgrammingAssignment2
## makeCacheMatrix:    This function creates a special "matrix" object that  
##			           can cache its inverse.
makeCacheMatrix <- function(CM = matrix()) {    
    invCM <- NULL                               
    set <- function(y) {                        
        CM <<- y                                                                
        invCM <<- NULL
    }
    get <- function() CM
    setinverse <- function(inverse) invCM <<- inverse
    getinverse <- function() invCM
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve:	This function computes the inverse of the special  
##			"matrix" returned by makeCacheMatrix above. 
##			If the inverse has already been calculated (and the 
##			matrix has not changed), then cacheSolve will 
##			retrieve the inverse from the cache.
cacheSolve <- function(CM, ...) {
    invCM <- CM$getinverse()
    if(!is.null(invCM)) {
        message("Getting inverse from cache.")
        return(invCM)
    }
    message("Calculating inverse.")
    m <- CM$get()
    s <- solve(m)
    CM$setinverse(s)
    s 	

}


