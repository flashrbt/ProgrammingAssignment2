##Date: 09/06/2016 
##Authoer: CQ Li
##
##This program has two functions: makeCacheMatrix() and cacheSolve(). 
##
## makeCacheMatrix() defines four methods for matrix. To use this function, simple call makeCacheMatrix() and return a matrix with four methods. another way to use it is assign a matrix using set method. 
  
##      cacheSolve() return the inverse of matrix value, if it is null, it will calculte the inverse of matrix value, save it and return the value back. 


 
#
##      makeCacheMatrix() creates a matrix "x" with four methods:
##          1. set, to set Matrix value "x"
##          2. get, to reture matrix value "x"
##          3. setInverse, save the inverse of matrix value, defined as "Inver"
##          4. getInverse, return the inverse of matrix value,defined as "Inver"
##          
         
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

}  ##end of function makeCacheMatrix()


## cacheSolve() return the inverse of matrix. if the inverse value is cached, it only return this cached value "Inver". Otherwise, this fuction call solve(x) to caculte the inverse value and save this value using setInverse() method.

cacheSolve <- function(x, ...) {
    
        ## Return a matrix "Inver" that is the inverse value of 'x'
    
    
    Inver<-x$getInverse()
    if(!is.null(Inver)){
            message('getting cached inverse of Matrix')
            return(Inver)
    }
    
     ## get matrix value of "x" and call solve() function to get inverse value "Inver",
    data<-x$get()
    Inver<-solve(data,...)
    
    # then set "Inver" by calling "setInverse()" function and return "Inver" that is the inverse value of "x". 
        
    x$setInverse(Inver)
    Inver
    
}   ## end of function cacheSolve()

##  Here is a example showing how to use these two functions: 
##  mdat is a predefined matrix
# > mdat
# [,1] [,2] [,3]
# [1,]  1.0  2.0  3.0
# [2,]  0.3  0.5  0.7
# [3,]  1.0  0.5  0.2
# > x1=makeCacheMatrix(mdat)
# > y1=cacheSolve(x1)
# > y1
# [,1] [,2] [,3]
# [1,]  12.5  -55    5
# [2,] -32.0  140  -10
# [3,]  17.5  -75    5
# 
# > round(y1%*%mdat,3)
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
# 
