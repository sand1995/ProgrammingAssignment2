## Put comments here that give an overall description of what your
## functions do


##the matrix is created

##> a<-c(0,1,1,1,0,0,0,0,1)
##> a2<- matrix(a,nrow = 3,ncol = 3,byrow = T)
##> a2
##[,1] [,2] [,3]
##[1,]    0    1    1
##[2,]    1    0    0
##[3,]    0    0    1

##the matrix is used in the function which will make the cache and is assigned to another variable
##> a3<-makeCacheMatrix(a2)

##the function is used to find the inverse matrix and to store its information in the previous function
##> cacheSolve(a3)
##[,1] [,2] [,3]
##[1,]    0    1    0
##[2,]    1    0   -1
##[3,]    0    0    1

##already stored the inverse matrix is avoided its calculation, obtaining it of the previous function
##> cacheSolve(a3)
##getting cached data
##[,1] [,2] [,3]
##[1,]    0    1    0
##[2,]    1    0   -1
##[3,]    0    0    1


## Write a short comment describing this function
##based on example 

makeCacheMatrix <- function(x = matrix()) {
  
  invM <- NULL
  ##add a new matrix and release the information of its matrix inverse
  set <- function(y){
    x<<-y
    invM<<-NULL
  }
  ##obtains the entered matrix
  get <- function() {x}
  
  ##save the Matrix inverse
  setInverseMatrix <-function(inv) {invM <<- inv}
  
  ##get the inverse matrix saved
  getInverseMatrix <-function() {invM}
  
  ##the list of functions to use
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix =getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##get the value of the inverse matrix of the cache
  invM<-x$getInverseMatrix()
  ##if there is information of the inverse matrix is shown
  if(!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
  ## but the inverse matrix is obtained and saved
  matriz<-x$get()
  invM<- solve(matriz)
  x$setInverseMatrix(invM)
  invM
}
