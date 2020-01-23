## defined x as matrix
## inverse set to null to be used later by makeCacheMatrix
## y is defined by x in the parent environment
## set the functions as solve to create the inverse of matrix
## named the elements and returned them in list form to the parent environment

makeCacheMatrix <- function(x = matrix()) {
	inverse<-NULL
	set<-function(y){
		x<<-y
		inverse<<-NULL
	}
	get<-function()x
	set_inverse<-function(solve)inverse<<-inverse
	get_inverse<-function()inverse
	list(set=set,get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## cacheSolve first checks to see if the solve fuction is null
## if it is null then it will perform the function solve on the matrix
## if it is not null then it will return the stored value of the stored inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$get_inverse()
        if(!is.null(inverse)){
        	message("getting cached data")
        	return(inverse)
        
        }
        data<-x$get()
        inverse<-solve(data,...)
        x$set_inverse(inverse)
        inverse
}
