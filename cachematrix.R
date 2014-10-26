# Kagadkar
# We use persistent state variables
# Note this is similar to Python closures

#
# Example Usage:
# m <- makeCacheMatrix( diag(3) )
# First computation, so it needs to perform solve()
# cacheSolve(m)   
# Subsequent commands access and retrieve from cache
# cacheSolve(m)   


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	# The set function initializes the raw matrix data state variable component of the
	# "cachedMatrix" object
	set <- function(z) {
		x <<- z		# Use superassignment to access x in parent (function) scope
		inv <<- NULL	# At initial assignment of "cachedMatrix" object, inv is not known
	}
	# get acts as an "access API" allowing to probe the raw matrix data contained in the "cachedMatrix"
	# object i.e. access persistent state variable x
	get <- function() x
	# Provide API to store new inv state variable, compututation happens outside of this procedure
	setInverse <- function(data) {
		inv <<- data
	}
	# Provide API to fetch inv state variable
	getInverse <- function() inv
	# Return list of the API functions!
	list( set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse )
}


# cacheSolve looks up persistent state elements internal to a special "cachedMatrix" object
# It uses the getInverse function to see if the inverse has been already computed and cached
# If it has, it presents the result of the cache
# Otherwise it is responsible for computing the inverse and updating the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	# Note 'x' is a cachedMatrix object that has been initialized outside of this function
	inverseMatrix <- x$getInverse()
	# If it has not been pre-computed, it will not have been cached as a state variable, and thus
	# will return null
	if( !is.null(inverseMatrix) ) {
		message("Fetching cached data")
		return(inverseMatrix)
	}
	# Otherwise compute now, and make sure to save up in the appropriate state variable on x
	data <- x$get()		# Fetch state variable containing raw matrix
	inv <- solve(data)	# Assume here, matrices are invertible
	x$setInverse(data)	# Cache up data as state variable on x
	inv			# Return value
}
