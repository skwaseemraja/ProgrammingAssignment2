# function to create a matrix and store the inverse when cached through cacheSolve function

makeCacheMatrix <- function(x) {
        if (class(x)=="matrix"){        # check the argument class and proceed if it is a matrix
                invm <- NULL            #initializing the inverse of matrix to NULL
                set <- function(y) {    # create the matrix and set the inverse as NULL
                        x <<- y
                        invm <<- NULL
                }
                get <- function() x     # returns the matrix
                setinv <- function(inversematrix) invm <<- inversematrix # reads the inverse matrix from cacheSolve function and stores it
                getinv <- function() invm #returns the cached inverse matrix when called
                list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)        # returns the
        }
        else{
                message("Input should be a matrix") # throws a message if the input is not a matrix
        }
        
}


# function to compute matrix inverse if the cache is not available 
# and to write to cache when called for the first time

cacheSolve <- function(x, ...) {

# if the inverse is already computed and cached, retrive it
        invm <- x$getinv()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm) # return the inverse matrix if available from cache
        }

# if the inverse is not cached and if the matrix is invertible,
# compute the inverse and cache it         
        matdata <- x$get()
        if (nrow(matdata)==ncol(matdata)){
                invm <- solve(matdata) # create inverse matrix
                x$setinv(invm) # write to cache
                invm # return the inverse matrix
        }
# if the matrix is not invertible, throw a message
        else {
                message("Matrix is not a square matrix, hence not invertible")
        }
}