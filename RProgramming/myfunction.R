myfunction <- function(y){
	y <- rnorm(200)
	mean(y)
}

second <- function(x){
	x + rnorm(length(x))
}