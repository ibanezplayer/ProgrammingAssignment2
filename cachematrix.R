## makeCacheMatrix takes a matrix and converts it into a cacheable object with get and set 
## functions which store and retreive a matrix in a separate environment for caching purposes
makeCacheMatrix <- function(x = matrix()) {
	#initialize the matrix
	cm <- NULL

	#define the set function as taking a parameter
	set<-function(y) {
		#cache the parameter into a separate environment
		x<<-y
		#reset the matrix
		cm<<-NULL
	}

	#define the get function to return the matrix
	get<-function() x

	#declare the setinverse function to store the matrix passed as a parameter 
	#into the cm variable in a separate environment
	setinverse<-function(cachematrix) cm<<-cachematrix

	#get the matrix current in memory
	getinverse<-function() cm

	#return the object as a list of functions
	list(set=set, get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


## cacheSolve calculates, stores, and returns the inverse of a given matrix if it does not exist,
## otherwise it returns the cached value
cacheSolve <- function(x, ...) {
	#get the current matrix from the object passed in
      cm<-x$getinverse()

	#if something was returned (returned object is not null), return the cached object & exit
	if(!is.null(cm)) {
		message("getting cached data")
		return =(cm)
	}

	#since nothing was returned, get the matrix from the object passed in
	data<-x$get()

	#invert the matrix by calling the solve function
	cm<-solve(data,...)

	#store the matrix in cache by passing it to the object's setinverse function
	x$setinverse(cm)

	#return the inverted matrix
	cm
}
