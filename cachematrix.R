## A pair of functions that cache the inverse of a matrix
> 
> 
> ## Creates a special matrix object that can cache its inverse
> ##there are two functions makeCacheMatrix, makeCacheMatrix
> library(MASS)
> makeCacheMatrix <- function( m = matrix() ) {
+   
+   ## Initialize the inverse property
+   i <- NULL
+   
+   ## Method to set the matrix
+   set <- function(y) {
+     m <<- y
+     inv <<- NULL
+   }
+   
+   ## Method the get the matrix
+   get <- function()m 
+   setinv<- function(inverse)inv<<-inverse
+   getinv<- function(){
+           inver<-ginv(m)
+           inver%*%m
+   }
+   list(set = set, get =get, 
+        setinv = setinv, 
+        getinv = getinv)
+   
+   
+ 
+ ##gets cache data
+ 
+ cacheSolve <- function(x, ...) {
+   
+   m <- x$getInv()
+   
+   
+   if( !is.null(m) ) {
+     message("getting cached data")
+     return(m)
+   }
+   
+   ## Get the matrix from our object
+   data <- x$get()
+   
+   ## Calculate the inverse using matrix multiplication
+   m <- solve(data) %*% data
+   
+   ## Set the inverse to the object
+   x$setInv(m)
+   
+   ## Return the matrix
+   m
+ }
