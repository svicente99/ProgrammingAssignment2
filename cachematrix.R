##  ----------------------------------------------------------
##  Coursera.org
## 
##	R Programming - Programming Assignment 2: solution 
##				  - Caching the inverse of a matrix
##
##  Student...:	Sergio Vicente (Niteroi, Brazil)
##  Twitter...: svicente99 (svicente99@yahoo.com)
##  Date......: Aug.21st 2014
##  ---------------------------------------------------------

##  Refs.: https://github.com/svicente99/ProgrammingAssignment2

##  To test this job do this way:

##  > mat <- matrix(1:4,2,2)		# make a matrix
##  > solve(mat)         		# check it has an inverse (not required if you're sure)
##  > x <- makeCacheMatrix(mat)		# run function to create an object to hold the cached data
##  > cacheSolve(x)			# this checks to see if the result is cached; if not, it caches it
##  > cacheSolve(x)		        # 2nd access so now we just fetch cached data
##  getting cached data           	# this is printed when the data is in the cache
##     	 [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

##  1st function:
##  creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x) {
        mat <- NULL
        set <- function(x) {					## function to assign values
                mat <<- x
        }
        get <- function() x					## function to return parameter value
        setSolve <- function(matr) mat <<- matr	
        getSolve <- function() mat
        list(set = set, get = get,
             setSolve = setSolve, getSolve = getSolve)
}

##  2nd function:
##  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        mat_inv <- x$getSolve()					## try to obtain inv.matrix of parameter
        if(!is.null(mat_inv)) {					## if already exists..
                message("getting cached data")
                return(mat_inv)					## just returns cached data
        }
        mat <- x$get()						## if not, get data of matrix
        mat_inv <- solve(mat, ...)				## calculate its inverse
        x$setSolve(mat_inv)					## set this value using cached function
        mat_inv
}
