## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function stores the inverse of a matrix
## So that it can be retrieved whenever needed
## without needing to re-calculate the inverse every time it is neede

makeCacheMatrix <- function(x = matrix()) {
	# The matrix inverse is initially NULL
	M_inverse <- NULL
	
	# This creates a function set(MyMatrix) which allows the user to store data which is in the form of a Matrx
	# And re-set the inverse of the Matrix to NULL
	set <- function(MyMatrix){
		x <<- MyMatrix
		M_inverse <<- NULL
	}
	
	# This creates a function which allows the user to retrieve the stored matrix
	get <- function() x

	# This creates a function which allows the user to store the inverse of the matrix
	# if it has already been calculated externally
	set_inverse <- function(inverse_in) M_inverse <<- inverse_in
	
	# This creates a function which allows the under to retrieve the stored inverse
	# which was previously stored by set_inverse
	get_inverse <- function() M_inverse

	# return a list of all the functions created eithin this function
	list( set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## Write a short comment describing this function
## This function checks to see if the inverse of the matrix has already been calculated
## and stored using the set_inverse function
## if so, then it simply returns the stored value
## Otherwise it calculates the inverse, stores it using set_inverse, and returns the inverse
cacheSolve <- function(x, ...) {
        # check to see if the inverse has been cached
	  m <- x$get_inverse()
	  if (!is.null(m)){
		message ("getted cached inverse of the matrix")
		return(m)
	  }
	  # if not, get the matrix and calculate the inverse using the solve() function
	  data <- x$get()
	  m <- solve(data)
	  x$set_inverse(m)
	  return(m)
}	  


