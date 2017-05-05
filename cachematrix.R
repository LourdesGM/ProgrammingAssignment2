## OWNER: Lourdes Gil Martínez
## DATE: 4th April 2017
##PURPOSE: to obtaine the inverse of a matrix checking first if it is already stored in cache

## This first function creates a type of object containing necessary inputs:
##the matrix, variables to store inverse,determinant, number col and number rows
##and the functions to obtain inverse,det, ncol and nrow from input matrix

makeCacheMatrix <- function(x = matrix()) {
    dt<-NULL
    inversa<-NULL
    nr<-NULL
    nc<-NULL
    set<-function(y){
        x<<-y
        dt<<-NULL
        inversa<<-NULL
        nr<<-NULL
        nc<<-NULL
    }
    get<-function()x
    setdt<-function(det)dt<<-det
    getdt<-function()dt
    setinv<-function(solve)inversa<<-inversa
    getinv<-function()inversa
    setnr<-function(nrow)nr<<-nrow
    getnr<-function()nr
    setnc<-function(ncol)nc<<-ncol
    getnc<-function()nc
    list(set=set,get=get,setdt=setdt,getdt=getdt,setinv=setinv,getinv=getinv,setnr=setnr,getnr=getnr,setnc=setnc,getnc=getnc)
}

## This functions returns the inverse of a matrix
## It gets the object produced in previous function which contains the matrix and extracts data needed
## It checks if solution already exists in cache an if so returns the value
## if it doesn's exist, we first assure matrix is invertible
## if matrix has an inverse, then it calculates and gives output
cacheSolve <- function(x, ...) {
    ## 'x' is the output of makeCacheMatrix and contains the matrix to invert
    dt<-x$getdt()
    inversa<-x$getinv()
    nr<-x$getnr()
    nc<-x$getnc()
     if(!is.null(inversa)){
        message("getting cache data")
            return(inversa)
     }
    #solution doesn't exist in cache
    #first calcualte if matrix is square, if not, stop
    data<-x$get()
    nr<-nrow(data,...)
    x$setnr(nr)
    nr
    nc<-ncol(data,...)
    x$setnc(nc)
    nc
    if (nr!=nc){
        message("matrix is not square, it is not invertible")
        return(inversa)
    }
    #check if matrix is singular
    #for that purpose, first get determinant from cache if already calculated and if not, calculate it
   if(is.null(dt)) {
        dt<-det(data,...)
        x$setdt(dt)
        dt
        print(dt)
   }
    
    if (dt==0) {
        message("matrix is singular, there is no inverse")
        return(inversa)
    }
    #matrix is invertible and inverse is not in cache, we calculate it
    inversa<-solve(data)
    x$setinv(inversa)
    inversa
    return(inversa)
}