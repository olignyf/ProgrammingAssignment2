## This file contains two functions whose purpose is computes the inverse of a 
## matrix on the first run and on subsequent runs returns the cached inverse
##
## Example Usage :
##> c <- makeCacheMatrix()
##> c$set(matrix)
##> cacheSolve(c)
##   [,1]      [,2]
##   [1,] 1.0666667 0.2666667
##   [2,] 0.2666667 1.0666667
##> cacheSolve(c)
##   getting cached data
##   [,1]      [,2]
##   [1,] 1.0666667 0.2666667
##   [2,] 0.2666667 1.0666667


## Function makeCacheMatrix
## Written : 27.Jul.2014
## Description : This will functions keeps two internal variable : i and x 
##      i -> saved inverse of the matrix
##      x -> the actual matrix
## Usage : 
##> c <- makeCacheMatrix()
##> c$set(matrix)
##> c$get(matrix)
##> c$setinverse(solve(matrix))
##> c$getinverse()
makeCacheMatrix <- function(x = matrix()) 
{
	i <- NULL
	
	set <- function(y) 
	{
		x <<- y
		i <<- NULL
	}
	get <- function() x
	
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	list(set = set, get = get,
		  setinverse = setinverse,
		  getinverse = getinverse)
}


## Function cacheSolve
## Written : 27.Jul.2014
## Input: 'x' which is of type makeCacheMatrix.
## Output: Returns the inverse of the matrix saved in 'x'.
## Description : Will compute the inverse of the matrix in 'x' and
##        save it into cache of 'x' on the first run. On subsequent 
##        run, will just get the cached version saved into 'x'.
## Usage :
##> c <- makeCacheMatrix()
##> c$set(matrix)
##> cacheSolve(c)
cacheSolve <- function(x, ...)
{
	inverse <- x$getinverse()
	
	if(!is.null(inverse)) 
	{
		message("getting cached data")
		return(inverse)
	}
	
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
