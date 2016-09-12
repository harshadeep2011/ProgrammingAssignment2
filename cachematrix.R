## Week3-Prgramming Assignment

## Caching the Inverse of a matrix

## Below Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inver<<- inverse
        getInverse <- function() inver
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Below function calculates the inverse of the special "matrix" created by above makeCacheMatrix 

## If the inverse has already been calculated,then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
inver<- x$getInverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setInverse(inver)
        inver
}

## Testing above Program

ownmatrix<-makeCacheMatrix(matrix(10:13,2,2))
ownmatrix$get()
ownmatrix$getInverse()
cacheSolve(ownmatrix)
