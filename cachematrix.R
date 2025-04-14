makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x<<-y
                inverse<<-NULL
        }
        get <- function() x
        setinv <- function(inv) inverse<<-inv
        getinv <- function() return(inverse)
        return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}


cacheSolve <- function(x=matrix(), ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)){
                print("Cached Data Exists...")
                return(inverse)
        } 
        solving <- x$get()
        inverse <- solve(solving)
        x$setinv(inverse)
        inverse
}
