## The functions take a matrix and produce its inverse, which is then stored.
## Only in case makeCacheMartix receives a matrix not identical with the previously loaded one the 
## cacheSolve function calculates a new inverse. Otherwise it hands over the stored inverse.

## The makeCacheMatrix function creates a list containing a function to
##    get the matrix
##    check if the matrix is identical with the one previously loaded
##    set the IM (identical matrix) flag
##    set the inverse
##    get the inverse


makeCacheMatrix <- function(x = matrix()) {
        IM <-FALSE #identical matrix flag
        if(exists("z")) {
                if(identical(z,x)==TRUE) IM<-TRUE
        }
        else z<<-x #cache the matrix so that it can be compared with next loaded matrix
        getmatrix <- function() x
        setinverse <- function(inverse) Inv <<- inverse
        comparematrix <- function() IM
        setIM<-function(identical) IM <<- identical
        getinverse <- function() {
                if(exists("Inv")) Inv
                else Inv<<-NULL
        }
        list(getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse,
             comparematrix = comparematrix,
             setIM = setIM)
}


## The following function calculates the inverse of the matrix handed to the above function. Before
## doing this it actually checks to see if the inverse has already been calculated for the respective ## matrix. If so, it takes the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse from the new matrix and sets the value of the inverse in the cache via the ## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinverse()
        IM<-x$comparematrix()
        if(!is.null(Inv)&IM==TRUE) {
                message("getting cached inverse")
                return(Inv)
        }
        data <- x$getmatrix()
        Inv <- solve(data)
        x$setinverse(Inv)
        x$setIM(TRUE)
        Inv
}