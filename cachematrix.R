## Reduces cost of computation by caching inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
    set<-function(y){
        x<<-y
        inverse<<-NULL
    }
    get <- function(){x}
    setinverse<-function(i){
        inverse<<-i
    }
    getinverse<-function(){inverse}
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned from the makeCacheMatrix.
## If the inverse has been calculated and matrix hasn't been changed, then cacheSolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    i<-x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data)
    x$setinverse(i)
    i
}
