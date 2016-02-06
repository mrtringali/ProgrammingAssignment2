## Programming Assignment 2
## M. Tringali
## Created 6 Feb 2016
## This is how it's done, folks.

### STEP 1: Write a function that creates a special "matrix" object that can
###         cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


### STEP 2: Write a function that computes the inverse of the special "matrix"
###         returned by the function above. If the inverse has already been 
###         calculated then the function should retrieve the inverse from the 
###         cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

### Step 3: TEST THE CODE
###         Create a 2x2 matrix then run the script to test the output. Test
###         output is below.

m <- makeCacheMatrix(matrix(1:4, 2))
m$get()
m$getinv()
cacheSolve(m)
m$getinv()
cacheSolve(m)
m$getinv()
x <- cacheSolve(m)
x

## TEST CODE OUTPUT
# > m <- makeCacheMatrix(matrix(1:4, 2))
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m$getinv()
# NULL
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m$getinv()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m$getinv()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > x <- cacheSolve(m)
# getting cached data
# > x
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

