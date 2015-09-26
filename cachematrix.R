####################
## WHAT'S HERE
#########################################
# Function 1: makeCacheMatrix(variable) ...  a function to initialize the matrix object
# Function 2: cacheSolve(matrix)        ...  a function for solving a matrix and storing the solution in the object
# Function 3: test.cacheMatrix()        ...  a rudimentary testing, otherwise I'll forget how this works
#########################################



###############################
## makeCacheMatrix(variable) ##
###############################
##  This function initializes a cache-matrix object using 'variable' as a dummy target
##  The object has four "methods" stored in a list the methods are accesed as
    # $set(matrix) setter method that puts a matrix to storage and clears the invverse matrix
    # $get() returns the matrix if it exists
    # $setInv(Mat) a setter method to store a matrix as an inversed matrix
    # $getInv() gets the matrix stored as an inverse matrix


makeCacheMatrix <- function(x = matrix()) {

    # initialization
    x_inverse <- NULL
    
    # A setter function
    set <- function(y) {
        x <<- y # this stores y in a globally scoped variable x
        x_inverse <<- NULL # a globally scoped variable x_inverse is created
    }
    
    # getter function
    get <- function() x # return the input matrix
    
    # setting the inverse 
    setInv <- function(inv) x_inverse <<- inv 
    
    # getting the inverse
    getInv <- function() x_inverse 
    
    # return a list that contains these functions, so that we can use
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}



########################
## cacheSolve(matrix) ##
########################
    # a function that returns the inverse matrix if the inversion has been done
    # otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
    # thought, the function should return error if the x object does not containt the proper object structure
    
    m <- x$getInv() # get the inversed matrix from object x (NULL if this is the first time)

    if(!is.null(m)) {
        message("getting cached data")
        return(m) # return the calculated inversion
    }
    
    # block if m was not NULL
    data <- x$get() #  get the matrix from the object
    m <- solve(data) # get the solve matrix (inverse) matrix
    x$setInv(m) # set the inverse into the object 
    m # return result
}




#########################################
# Function 1: makeCacheMatrix(variable) ...  a function to initialize the matrix object
# Function 2: cacheSolve(matrix)        ...  a function for solving a matrix and storing the solution in the object
# Function 3: test.cacheMatrix()        ...  a rudimentary testing, otherwise I'll forget how this works
#########################################



########################
## test.cacheMatrix() ##
########################
    # a function a function that tests/demonstrates how the function worsk
    
test.cacheMatrix <-function() {
    # in leu of proper unit testing package
    
    # Test
    message("------------------------")
    message("--- start of testing ---")
    message("------------------------")
    message("")
    
    message("initializing a blank matrix")
    message("mat <- makeCacheMatrix(foo)")
    mat <- makeCacheMatrix(foo)
    message("")
    
    message("--Matrix length should be 4")
    message(if(length(mat)==4){"  pass"}else{"  fail"})
    
    message("--The matrix should have blank inversed matrix")
    message(if(is.null(mat$getInv())){"  pass"}else{"  fail"})
    
    message("")
    message("Setting a matrix")
    message("xmat <- matrix(runif(16,1,100),4,4)")
    message("mat$set(xmat)")
    xmat <- matrix(runif(16,1,100),4,4)
    message("")
    
    message("--A matrix is stored correctly")
    mat$set(xmat)
    message(if(all.equal(mat$get(), xmat)){"  pass"}else{"fail"})
    xmat <- matrix(runif(16,1,100),4,4)
    message(if(all.equal(mat$get(),xmat)!=1){"  pass"}else{"fail"})
    
    message("--Inverse of the new matrix does not exists")
    message(if(is.null(mat$getInv())){"  pass"} else {"  fail"})
    
    message("")
    message("Calculating the inverse:")
    message("mat_inv <- cacheSolve(mat)")
    message("")
    
    mat_inv <- cacheSolve(mat)
    EE <- round(mat$get() %*% mat_inv ,4)
    
    message("--calculation was correct")
    message(if(all.equal(EE, diag(4))){"  pass"}else{"  fail"})
    
    message("--the matrix is properly stored")
    all.equal(mat$getInv(), diag(4))
    message(if(all.equal(mat$getInv(), mat_inv)){"  pass"} else {"  fail"})
    
    message("-- the matrix inverse is not re-calculated (getting cached data three times)")
    message("--------------")
    mat_inv <- cacheSolve(mat)
    mat_inv <- cacheSolve(mat)
    mat_inv <- cacheSolve(mat)
    message("--------------")
    message("")
    
    message("Resetting the matrix")
    message("xmat <- matrix(runif(16,1,100),4,4)")
    message("mat$set(xmat)")
    message("")

    xmat <- matrix(runif(16,1,100),4,4)
    mat$set(xmat)
    
    message("--A new matrix is stored correctly")
    message(if(all.equal(mat$get(), xmat)){"  pass"}else{"fail"})
    
    message("--Inverse of the new matrix does not exists")
    message(if(is.null(mat$getInv())){"  pass"} else {"  fail"})
    
    message("Inverse is calculated (no getting cached data)")
    mat_inv <- cacheSolve(mat)
    EE <- round(mat$get() %*% mat_inv ,4)
    
    message("--calculation was correct")
    message(if(all.equal(EE, diag(4))){"  pass"}else{"  fail"})
    
    message("--the matrix is properly stored")
    all.equal(mat$getInv(), diag(4))
    message(if(all.equal(mat$getInv(), mat_inv)){"  pass"} else {"  fail"})
    
    message("-- the matrix inverse is not re-calculated (getting cached data once)")
    message("--------------")
    mat_inv <- cacheSolve(mat)
    message("--------------")
    
    message("")
    message("------------------------")
    message("---  end of testing  ---")
    message("------------------------")
    message("")
    }
