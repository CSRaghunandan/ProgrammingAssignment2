# creates a special object, which really is a list containing a function to:-
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value inverse of the matrix
makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    # set the value of the matrix and set the inverse of the matrix to NULL 
    # to indicate that the mean of that matrix has not yet been calculated.
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    # fetch the value of the matrix.
    get <- function() x
    # cache the value of inverse of the matrix.
    setInverse <- function(inverse) inv <<- inverse
    
    # fetch the cached value of inverse of the matrix.
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Calculates inverse of the matrix, but first checks to see if the matrix 
# inverse has already been solved, if so, the function fetches the data
# from the cache and skips the computation.
cacheSolve <- function(x, ...)
{
    # fetch the value of the inverse of the matrix.
    inv <- x$getInverse()
    
    # if the matrix has already been solved, getInverse would return a value
    # and !is.null(inv) would be true and thus, will return the computed value 
    # from the cache 
    if(!is.null(inv))
    {
        message("Getting chached data")
        return(inv)
    }
    
    # if the matrix inverse, had not been solved, 
    #we first get the original matrix
    data <- x$get()
    
    # compute the inverse of the original matrix
    inv <- solve(data, ...)
    # cahce the result of the computation, so that we can use it again.
    x$setInverse(inv)
    # return the cached result (i.e, the inverse of the matrix)
    inv
}
