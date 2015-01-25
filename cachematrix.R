## My first function creates a list with the matrix object embedded in it.
## This matrix object has getters and setters which enables caches.

## You interact with it via the getter setter API's.  A slight change is that there is a flag which, if called, will populate the 

makeCacheMatrix <- function(x = matrix(), computeNow = 0) {
        require(MASS)
		
		inv <- NULL
		if(computeNow == 1) {
                message("creating inverse now")
                inv <- solve(x)
        }	
		
        set <- function(y,computeNow = 1) {
                x <<- y
                m <<- NULL
				if(computeNow == 1) {
						message("creating inverse now")
						inv <<- solve(x)
				}	
				
        }
		
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        #Output
		list(
			set = set,
			get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## If the computeNow option is not called in the initial setup of the cachematrix object
## The inverse is then calculated in this function and set in the original object

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}