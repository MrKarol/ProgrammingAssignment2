## function makeCacheMAtrix stores in its environment a matrix + its inverse
## the first time we call it with a new matrix then an inverse is set to NULL
## to calculate an inverse of a matrix stored in makeChaceMatrix we need to go via cacheSolve function
## the whole problem could be resolved differently
## However, we were asked to use two functions to learn about lexical scoping and free variables
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set function - stores new matrix in the makeCacheMatrix environment and calculates its inverse
        set <- function(y) {
            ## we use <<- because we assign a value to x in the parent environment (makeCacheMatrix environment)
            x <<- y         ## new matrix stored 
            inv <<- NULL    ## we just have stored new matrix so inverse we set to NULL
        }
        ## get function returns a matrix stored in makeCacheMatrix
        get <- function() {
            x
        }  
        ## set function sets an inverse of a matrix x; setinv is called from cacheSolve (function that calculates inverse)
        setinv <- function(y) {
          inv <<- y
        }
        ## get function returns an inverse of a matrix stores in makeCacheMatrix
        getinv <- function() {
            inv
        }
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve function calculates (if matrix not in cache) and returns an inverse of a matrix stored in  makeCacheMatrix environment
## if an inverse of matrix exists in makeCacheMatrix environment then it will be returned
## else new inverse will be calculated, stored in makeCacheMatrix environment and returned
## argument x is a list returned by makeCacheMatrix
## we need to call makeCacheMArix before calling cacheSolve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', if inverse was not calculated yet then NULL is returned
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached matrix from makeCacheMatrix environment")
            return(inv)
        }
        ## the below code runs if we do not have an inverse in the makeCacheMatrix environment (inverse not-cached yet)
        data <- x$get()     #getting matrix stored in the makeCacheMatrix environment
        inv <- solve(data)  #calculating matrix inverse
        x$setinv(inv)       #storing matrix inverse inside makeCacheMatrix environment (caching)
        inv                 #inverse returned
}
