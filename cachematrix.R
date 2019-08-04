makeCacheMatrix <- function(x = matrix()) {
        ix<-NULL #ix is the inverse matrix of matrix x, and is set to NULL untill run "cacheSolve".
        set<-function(y){
               x<<-y
               ix<<-NULL
       }#set the value of matrix and its inverse matrix
        get<-function() x#get the value of matrix
        setinv<-function(inv) ix<<-inv #set the value of inverse matrix
        getinv<-function() ix #get inverse of "x"
        list(set=set,get=get,
             setinv=setinv,
             getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ix<-x$getinv()
        #If we have cached a inverse of "x", read it.
        if(!is.null(ix)){
                message("getting cached data")
                return(ix)
        }
        #If we do not have a inverse, calculate and cache it now.
        data<-x$get()
        ix<-solve(data)
        x$setinv(ix)
        message("calculatting data")
        ix
        ## Return a matrix that is the inverse of 'x'
}
x<-matrix(c(1,0,0,0,1,0,0,0,4),3,3)
aInv<-makeCacheMatrix(x)
cacheSolve(aInv) # message("calculatting data")
cacheSolve(aInv) # message("getting cached data")