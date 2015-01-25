##These two functions take in a matrix and return its
##inverse.


## makeCacheMatrix is a function that takes in a matrix x
##and returns a list of four functions, a special matrix.

makeCacheMatrix <- function(x = matrix()) {
        matInv<-NULL
        set<- function(y) {
                x<<-y
                matInv<<-NULL
        }
        get<-function() x
        setmatInv <- function(invMat) matInv<<-invMat
        getmatInv <- function() matInv
        list(set=set,get=get,
             setmatInv=setmatInv,
             getmatInv=getmatInv)
}


## cacheSolve uses the special matrix and finds out whether the
##inverse has already been computed, and if not it finds the inverse
##with the solve() function

cacheSolve <- function(x, ...) {
        matInv<-x$getmatInv()
        if(!is.null(matInv)){
                message("getting cahed data")
                return(matInv)
        }
        data<-x$get()
        matInv<-solve(data,...)
        x$setmatInv(matInv)
        matInv
        ## Return a matrix that is the inverse of 'x'
}
