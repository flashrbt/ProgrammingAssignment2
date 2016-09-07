##Date: 09/06/2016 
##Authoer: CQ Li
##This program has two functions: makeCacheMatrix() and cacheSolve(). 
##      makeCacheMatrix() creates a matrix with four methods:
##          1. set, to set Matrix value
##          2. get, to reture matrix value
##          3. setInverse, change the inverse of matrix value
##          4. getInverse, reture the inverse of matrix value
##      cacheSolve() reture the inverse of matrix value, if it is null, it will calculte the inverse of matrix value, save it and return the value back. 

## Write a short comment describing this function
## 
## makeCacheMatrix() defines four methods for matrix. To use this function, simple call makeCacheMatrix() and return a matrix with four methods. another way to use it is assign a matrix using set method. 

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


## cacheSolve() return the inverse of matrix. if the inverse value is cached, it only return this cached value, otherwise, this fuction call solve(x) to caculte the inverse value and save this value using setInverse() method.

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
