oddcount <- function(x){
  x1 <- (x%%2 == 1)
  x2 <- x[x1]
  return(length(x2))
}