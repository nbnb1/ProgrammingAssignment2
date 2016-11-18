## 1. [Function] 
## Show the invese metrix of any square invertible matrix

## 2. [How to use] 

## **ma = any square invertible matrix** 

## First use: 
## a <- makeCacheMatrix(ma) cacheSolve(a) 
## cacheSolve(a)

## Ensuing use:
## a@set(ma)
## cacheSolve(a)

## 3. [Basic Principle] 
## a) Such a design is used to minimize computation power consumption
## b) The first function (makeCacheMatrix) creates a "matrix" object 
## that can cache its inverse. 
## c) The second function (cacheSolve) first retrieve the inversed 
## matrix saved in the first function, if there is any. Then it will
## calculate the invese matrix in the case when there is no saved 
## matrix available

## 4. [Credit]
## The majority of programing idea is borrowed from Roger D. Peng,
## PhD, Associate Professor @ Biostatistics @ Bloomberg School of 
## Public Health @ Johns Hopkins University. 
## (https://github.com/rdpeng/ProgrammingAssignment2)

## Any inquiries? Contact my @ https://github.com/nbnb1


## makeCacheMatrix creates a "matrix" object that can cache its 
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinvese <- function(solve) i <<- solve
    getinvese <- function() i
    list(set = set, get = get,
         setinvese = setinvese,
         getinvese = getinvese)
}


## cacheSolve 1) first retrieve the inversed matrix saved in the 
## first function, if there is any, or 2) in the case when there 
## is no saved matrix availableThen it will calculate the invese 
## matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinvese()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinvese(i)
    i
}

