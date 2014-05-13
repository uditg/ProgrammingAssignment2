##	This code consists of two functions. The first one is used to generate a matrix in
##	a format which has setters and getters for the matrix and its inverse. The second function
##	is designed to work on the type of objects returned from the first function and return the inverse
##	of the matrix.
##	Example Run:
##			> mat <- makeCacheMatrix(matrix(c(1, 1, 1, -1),2,2))
##			> mat$get()
##			     [,1] [,2]
##			[1,]    1    1
##			[2,]    1   -1
##			> mat$getinverse()
##			NULL
##			> cacheSolve(mat)
##			     [,1] [,2]
##			[1,]  0.5  0.5
##			[2,]  0.5 -0.5
##			> mat$getinverse()
##			     [,1] [,2]
##			[1,]  0.5  0.5
##			[2,]  0.5 -0.5
##			> cacheSolve(mat)
##			getting cached data
##			     [,1] [,2]
##			[1,]  0.5  0.5
##			[2,]  0.5 -0.5



## 	This function is used to generate a matrix in a format which has setters and getters
##	for the matrix and its inverse. This type of object stores the original matrix as well as its
##	inverse. The inverse is calculated the first time it is needed and then can be stored as a part of
## the object itself for future reference.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


##	This function is designed to work on the type of objects returned from the first function.
##	This returns the inverse the matrix. If there is no inverse stored it calculates it and stores it for
##	when it is needed next time. Otherwise the cached value is returned.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv        ## Return a matrix that is the merse of 'x'
}
