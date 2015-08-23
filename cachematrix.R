## Cachematrix 2.0
## Jakub Weiner, 23.8.2015
## Contact: weiner@mail.muni.cz

## This file consists of two functions, where the first one (makeCacheMatrix) generates
## a matrix of random distribution variables and the second one calculates its inversion,
## haven't it been done yet.
##
## See the introductions, attached to the functions itselves, for details.  

## 1) makeCacheMatrix
## makeCacheMatrix is a function, which generates symmetric matrix of random numbers
## within the normal distributions. It takes only argument, 'dims', which are 
## the square matrix dimensions with a default of 3.
## The function also measures and prints the time of own execution.

makeCacheMatrix <- function(dims = 3) {
        start.time <- Sys.time() 
      mat <<- matrix(rnorm(dims*dims), ncol = dims, nrow = dims)
      inv <<- NULL 
      mat_dims <<- dims ## a control element for the second function (see below)
      message("The matrix has been saved as 'mat'")
      end.time <- Sys.time()
      time.taken <- difftime(end.time, start.time, units = "secs")
      message("This operation took me ", time.taken, " seconds.")
}

## 1) cacheSolve
## cacheSolve function then calculates the inverse, if it haven't been done yet.
## The inverse matrix is then printed out, but only if the dimensions
## are equal less 30 (otherwise the printing out is only offered).
## The function also measures and prints the time of own execution.
## There are no arguments needed for this function.

cacheSolve <- function() {
        start.time <- Sys.time()
        if(!is.null(inv) & (mat_dims <= 30)) { 
                message("It looks like I already know something about your problem...")
                return(inv)
        }
        else if(!is.null(inv) & (mat_dims > 30)) { 
                message("It looks like I already know something about your problem...")
                message("WOW! This looks like quite a big deal! If you don't want to keep it secret, just call 'inv'")
        }
        else if (is.null(inv) & (mat_dims > 30)) {
                message("Your deligthful matrix inversion is about to be calculated now!")
                inv <<- solve(mat)
                message("WOW! This looks like quite a big deal! If you don't want to keep it secret, just call 'inv'")
               }
        else  {
                message("Your deligthful matrix inversion is about to be calculated now!")
                inv <<- solve(mat)
                return(inv)
        }   
        end.time <- Sys.time()
        time.taken <- difftime(end.time, start.time, units = "secs")
        message("This operation took me ", time.taken, " seconds.")    
}


