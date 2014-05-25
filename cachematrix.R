## makeCacheMatrix: return a list of functions for the matrix and the inverse:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL ## m Will store the cached inverse matrix

    set <- function(y) { 
        x <<- y
        m <<- NULL
    } ## Will set the matrix
    
    get <- function() x ## Will get the matrix

    setinv <- function(inverse) m <<- inverse ## Will set de inverse
    
    getinv <- function() m # Will get the inverse

    list(set = set, get = get, setinv = setinv, getinv = getinv) ## Create the list of functions
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) { ## Return the inverse
    m <- x$getinv() ## Get inverse value

    if (!is.null(m)) {
        message("getting cached data")
        return(m) ## Return inverse value if it exits
    }

    data <- x$get()
    m <- solve(data, ...) ## If the inverse is not calculated, calculate it

    x$setinv(m) ## Cache the inverse

    m ## Return the inverse
}
