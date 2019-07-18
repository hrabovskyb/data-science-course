add2 <- function(x,y){
  x + y
}

aboven <- function(x,n=10){
  use <- x > n
  x[use]
}

columnMean <- function(x, removeNA = TRUE) {
  nc <- ncol(x)
  means <- numeric(nc) ##numeric vector to store the means in
  
  for(i in 1:nc){
    means[i] <- mean(x[,i], na.rm = removeNA)
  }
  means
}