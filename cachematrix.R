## 
## The following 2 functions create a matrix a cacheable inverse 
## after the test code
##

## Creates a matrix and exports set and get methods and setsolve and getsolve to get the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(inv) m <<- inv
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
  


## Returns a inverse matrix. If already calculated will return a cached copy 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}

cm$setsolve(NULL)
cm$set(NULL)
## creates a 3x3 invertible matrix 
im <- matrix(1:9, nrow=3, ncol=3)
im[2,2] <- -9
## prints it
im

## create a cacheable matrix
cm <- makeCacheMatrix()
cm$set(im)
## Calculate the inverse
inv <- cacheSolve(cm)
## prints the inverse
inv

## Calculate the inverse we should see "getting cached data" 
inv <- cacheSolve(cm)
## prints the inverse
inv

## multiplies the inverse with the matrix we expect the identity matrix
ident <- inv %*% im
## prints the identity matrix
ident

