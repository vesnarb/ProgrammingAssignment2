## makeCacheMatrix creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL  
        set<- function(y){
                x<<- y
                i<<- NULL
        }
        get<-function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        }
        
## cacheSolve calculates the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<- solve(data, ...)
        x$setinverse(i)
        i
        }

        ## Return a matrix that is the inverse of 'x'

##Testing the Program:

m<-matrix(1:4,nrow=2,ncol=2) ## m is an invertible matrix defined by the user
x<-makeCacheMatrix(m) #creates the matrix object that can cache its inverse
x$get()
inv<-cacheSolve(x) #calculates the inverse  
inv
inv<- cacheSolve(x)


