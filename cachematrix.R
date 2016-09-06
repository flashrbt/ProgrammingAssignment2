## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    Inver<-NULL
    set<-function(y){
        
        x<<-y
        Inver<<-NULL
    }
    get<-function() x
    setInverse<-function(Inverse) Inver<<-Inverse
    getInverse<-function() Inver
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inver<-x$getInverse()
    if(!is.null(Inver)){
            message('getting cached inverse of Matrix')
            return(Inver)
    }
    data<-x$get()
    Inver<-solve(data,...)
    x$setInverse(Inver)
    Inver
}
