## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function takes advantage of lexical scoping in R
## and stores the value of a given matrix and, when combined with
## cachesolve, also its inverse within the function environment
## and then, returns the value to the parent environment 
## avoiding having to re-compute the inverse 

## let's go for it
## giving as default value an empty matrix, then no need to initialize m

makeCacheMatrix <- function(m = matrix()) {

## sets the value of minverse to NULL so, each time a new matrix is 
## passed as argument to makeCaheMatrix the inverse stored in the 
## function environment is cleaned to NULL

minverse <- NULL

## the function set is the setter, as clearly explained by lgreski in
## the article Demystifying makeVector() this setter does two things
## Assign the input argument to the m object in the parent environment, and
## Assign the value of NULL to the minverse object in the parent environment
## so it clears any value of minverse that had been cached by a prior execution of cacheSolve() 

set <- function(y) {
m <<- y
minverse <<- NULL
}

## the get function when called in the parent environment 
## retrieves the value of m stored in the makeCacheMatrix()
## environment

get <- function() m

## setinverse sets the inverse of the matrix and 
## stores its value en the parent environment

setinverse <- function(inverse) minverse <<- inverse

## get inverse when called from the parent environment
## retrieves the value of minverse stored in makeCacheMatrix()
## environment

getinverse <- function() minverse

## finally, makeCacheMatrix() gives an object, a list
## with the four functions as its elements

list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}





## Write a short comment describing this function

## this function will check if the inverse of the matrix passed to 
## makeCacheMatrix() (or set by the set function within
## makeCacheMatrix) is already stored in the cache and if so
## it will retrieve the stored data to the parent environment
## if not, it will calculate the inverse and store it in 
## makecachematrix() environment

## cacheSolve take as argument an object x of the type makeCacheMatrix()

cacheSolve <- function(x, ...) {

## first gets the inverse stored in the environment of makecachematrix()
## and checks if the value is not NULL (si the matrix has been passed
## and the inverse is stored and there is no need to re-calculate it
     
	minverse <- x$getinverse()
	if(!is.null(minverse)) {
	
	## returns the value stored in the environment of makecachematrix()
	## to the parent environment
	
		message("getting cached data")
		return(minverse)

	}

## if the value of minverse stored in the makecachematrix() environment is 
## NULL then a new matrix has been set so it calculates the inverse of such
## new matrix by first getting the value and storing it in newmatrix

	newmatrix <- x$get()

## then storing it in minverse

	minverse <- solve(newmatrix, ...)

## then storing it within makecachematrix() environment using setinverse

	x$setinverse(m)

## then returning the value of the inverse to the parent environment

	minverse

}