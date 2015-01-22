# Here the main purpose is to create two functions for caching the inverse of matrix.

# (1) The first function takes an entry of matrix, and create a list of functions to be used later 
# for the purpose of caching the inverse of matrix.

makeCacheMatrix <- function (mat = matrix())
  {
     inv_mat <- NULL
  
     set <- function (other_matrix)   #assign the argument to the matrix
       {
          mat <<- other_matrix
          inv_mat <<- NULL
       }
  
     get <- function ()   #Returns the value for the matrix
       {  
          return(mat)  
       }
     
     setinverse <- function(inverse_matrix)   #assign the argument to the inverse matrix
       {
         inv_mat <<- inverse_matrix
       }   
  
      getinverse <- function ()   #Returns the value for the inverse matrix
        { 
          return (inv_mat)
        }    
  
    #outputs the list of functions defined above
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }


#(2) The second function defines a function which is applied to the function list for finding 
#the inverse of matrix

cacheSolve <- function (FunList, ...) 
  {
     #set inv_mat to the existing value of inverese matrix
      inv_mat <- FunList$getinverse() 
  
     #If there is a value currently stored in inv_mat then return it
       if(!is.null(inv_mat)) 
         {
            message("getting cached data")
            return(inv_mat)
         }
  
     #set the data to calculate a new inverse of matrix, and returns the value of matrix    
      data <- FunList$get()
  
     #calculate a new inverse of matrix based on the data using solve() fanction
      new_inverse <- solve (data)
  
     #Set this new value for the inverse of matrix to inv_mat
      FunList$setinverse (new_inverse)
  
     #output the value of the inverese of matrix
      inv_mat
  }
  
