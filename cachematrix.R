#First I am making the special matrix (it will be an input that i specify in the prompt), 
#with functions that will set the matrix and that will get the matrix. 
# In addition it will have function that will set the inverse of the matrix and a function
# that will get the inverse of the matrix.


makeCacheMatrix <- function(x=matrix())  {
	i <- NULL
	set <- function(y)   {
		x <<- y
		i <<- NULL
	}
	get <- function(x)
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# In the code below, I can calculate the inverse of my matrix.If the matrix have been inverted
# earlier, then the program code below will just "open" up the inverted matrix that have been
# calculated earlier, in stead of inverting the matrix all over again. Thereby saving time.
# This is what is done in the part of the code that where it stands "if(!is.null....). 
# If the matrix have not been inverted before, then in the last few lines of the program
# code, the inverse of the matrix is calculated.

cacheinverse <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matri <- x$get()
        i <- solve(matri, ...)
        x$setinverse(i)
        i
}

# End of the program code