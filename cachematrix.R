## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creats a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(new_mat){
                x <<- new_mat
                mat_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) mat_inv <<- inv
        getinv <- function() mat_inv
        
        as.matrix(list(set=set,get=get,setinv=setinv,getinv=getinv))
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x['getinv',1][[1]]()
        if(!is.null(mat_inv)){
                message("getting cached data")
                return(mat_inv)
        }
        data <- x['get',1][[1]]()
        mat_inv <- solve(data, ...)
        x['setinv',1][[1]](mat_inv)
        mat_inv
}
