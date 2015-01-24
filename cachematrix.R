## These functions take in a matrix and if the inverse was allready calculated, it can return that cached inverse matrix.
##Otherwise it can calculate the inverse with the solve() function and cache it at that point. Then it can be called in the future.

## The make CacheMatrix function stores the matrix in the global enviornment (cached). 
##The function also makes it possible the get the matrix, save the inverse of the matrix, pull out the inverse of the matrix and list all the elements within  the function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## i is the inverse of the matrix
        
        ## Test code to understand the enviornments#########
        print(environment())
        evn <- environment()
        print(parent.env(evn))
        #####################################################
        
        set <- function(y) {
                x <<- y   ## stores the input matrix into parent enviornment "cached"
                i <<- NULL ## stores empty inverse matrix value in the global enviornment
        }
        get <- function() x ## this can be called by my_matrix$get() where my_matrix<-makeCacheMatrix(x), it returns the matrix
        
        setinverse <- function(inverse) {
                i <<- inverse #can be called to define/set inverse of matrix (not calculated by solve)
        }
        
        getinverse <- function() i # returns the inverse value
        
        
        ######test code to understand the enviornment########
        getevn<- function() environment()
        #####################################################
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             getevn = getevn) # list of methods that can be called?
        
}


## The cacheSolve function takes in a matrix and if determines if there is allready an inverse matrix cached.
## if there is an inverse cached it will return that inverse matrix, otherwise it will calculate the inverse of a matrix with the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()       ## sets i equal to the inverse if it has allready been cached, if not cached i=null                      
        if(!is.null(i)) {         ## if there is a cached value if will print "getting cached data" and return that value
                message("getting cached data")
                return(i)       ## when this returns a value it exits the funciton so that it will not execute the next 4 lines below.
                print ("'i' should print above this line")
        }
        data <- x$get()         ## if not cached it sets the original matrix equal to data
        i <- solve(data, ...)    ### this is where the actual inverse is calculated if it is not cached
        x$setinverse(i)         ##caches the inverse so that next time it is called it will not need to be calculated.
        i        
}