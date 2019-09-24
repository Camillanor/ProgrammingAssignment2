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