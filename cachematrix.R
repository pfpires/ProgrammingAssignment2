## Put comments here that give an overall description of what your
## functions do

##
## Creates a matrix cache that can store a matrix and it's inverse
##
## mat and inv are local variables in the environment of the outer most function,
## mat because it's in the formal argument list, and inv because it is explicitly set
## to null at the begining.
##
## the setter functions, when called (in cacheSolve), use the <<- operator, that will
## search for the symbol being assigned to in the parent enviroments. Since R
## uses lexical scoping, the parent environment is the enviroment where the 
## function was defined (the outermost function here), not the enviroment where it will 
## be called later (cacheSolve).
## 
## the getter functions, when called, will return the symbols inv and mat, which will be 
## bound to inv and mat in the envoronment of the function where they were defined 
## (lexical scope again), i.e., the outermost function here.
##
makeCacheMatrix <- function(mat = matrix()) {
    # mat and inv are local variables in the envioronment of this function
    inv <- NULL 
    
    set <- function(newMatrix) {
        mat <<- newMatrix           # assigns to mat in the parent environment
        inv <<- NULL                # assigns to inv in the parent environment
    }
    
    get <- function() {
        # when executed, will be bound to mat in the perent environment (lexical scope)
        # and then returned
        mat
    }
    
    setinv <- function(newInv){
        inv <<- newInv
    }
    
    getinv <- function() {
        inv
    }
    
    # returns a list of all the functions defined in this function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##
## Returns the inverse of the matrix x.
##   Checks if the inverse was already calculated and cached
##      if it was, returns the cached inv matrix
##      if it wasn't it calculates the inverse, caches it in inv and returns it
##
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
