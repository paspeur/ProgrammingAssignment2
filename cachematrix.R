# Coursera, R Programming, December 2014

# Assignment 2: Caching the Inverse of a Matrix

# Motivation: Matrix inversion is usually a costly computation and their may
# be some benefit to caching the inverse of a matrix rather than compute it
# repeatedly.  This assignment is to write a pair of functions that cache
# the inverse of a matrix.

# This file provides the following functions:

# * makeCacheMatrix: This function creates a special "matrix" object that
# caches its inverse.

# * cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above.  If the inverse has already been
# calculated (and the matrix has not changed), then the function retrieve the
# inverse from the cache.

###

# makeCacheMatrix: This function creates a special "matrix" object that
# caches its inverse.

makeCacheMatrix <- function(matrix_to_inverse = matrix()) {
	# At object creation, the matrix to inverse is passed by parameter to
	# the function but the cached inverted matrix is not set.
	cached_inverted_matrix <- NULL

	# The following four functions are setters and getters to the matrix
	# to inverse and the cached inverted matrix

	setMatrixToInverse <- function(updated_matrix_to_inverse) {
		matrix_to_inverse <<- updated_matrix_to_inverse

		# With an updated matrix to inverse, the previous cached
		# inverted matrix is invalidated
		cached_inverted_matrix <<- NULL
	}

	getMatrixToInverse <- function() {
		matrix_to_inverse
	}

	setCachedInvertedMatrix <- function(inverted_matrix) {
		cached_inverted_matrix <<- inverted_matrix
	}

	getCachedInvertedMatrix <- function() {
		cached_inverted_matrix
	}

	list(setMatrixToInverse = setMatrixToInverse,
		getMatrixToInverse = getMatrixToInverse,
		setCachedInvertedMatrix = setCachedInvertedMatrix,
		getCachedInvertedMatrix = getCachedInvertedMatrix)
}

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above.  If the inverse has already been
# calculated (and the matrix has not changed), then the function retrieve the
# inverse from the cache.

cacheSolve <- function(m, ...) {

	# Retreive the inverted matrix if it was previously cached

	inverted_matrix <- m$getCachedInvertedMatrix()
	if (!is.null(inverted_matrix)) {
		message('Fetching cached data')
		return(inverted_matrix)
	}

	# Otherwise, compute the inverse and cache it before returning it

	matrix_to_inverse <- m$getMatrixToInverse()
	inverted_matrix <- solve(matrix_to_inverse, ...)
	m$setCachedInvertedMatrix(inverted_matrix)
	inverted_matrix
}

# Sanity checking below

testCachedMatrix <- function() {
	matrix_to_inverse <- matrix(1:4, nrow = 2, ncol = 2)

	m <- makeCacheMatrix(matrix_to_inverse)

	# Check that the cached inverted matrix is null
	stopifnot(is.null(m$getCachedInvertedMatrix()))

	# Get the inverted matrix and check its correctness
	inverted_matrix <- cacheSolve(m)
	stopifnot(identical(inverted_matrix, solve(matrix_to_inverse)))

	# Do it a second time and manually observe that you get a message back along
	# with the inverted matrix
	inverted_matrix <- cacheSolve(m)
	stopifnot(identical(inverted_matrix, solve(matrix_to_inverse)))

	matrix_to_inverse <- matrix(4:1, nrow = 2, ncol = 2)

	# Update the matrix to inverse
	m$setMatrixToInverse(matrix_to_inverse)

	# Check that the cached inverted matrix as reset to null
	stopifnot(is.null(m$getCachedInvertedMatrix()))

	# Get the inverted matrix and check its correctness
	inverted_matrix <- cacheSolve(m)
	stopifnot(identical(inverted_matrix, solve(matrix_to_inverse)))
}

# testCachedMatrix()

