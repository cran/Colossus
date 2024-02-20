## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)


## ----eval=TRUE----------------------------------------------------------------
t <- c(1,2,5,6,7)
r <- c(4,3,5,4,2)
d <- c(1,2,3,2,1)

lambda  <- c()
Lambda <- c(0)
S      <- c(1)
T      <- c(0)

for (i in 1:5){
	lambda <- c(lambda, d[i]/r[i])
	Lambda <- c(Lambda, Lambda[i] + lambda[i])
	S     <- c(S, exp(-1*Lambda[i+1]))
	T     <- c(T, t[i])
}

dft <- data.table("x"=t,"y"=lambda)
g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$x, y=.data$y)) + 
     ggplot2::geom_point(color="black") +
     ggplot2::labs(x="age", y="Instantaneous Hazard")
g

dft <- data.table("x"=T,"y"=Lambda, "z"=S)
g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$x, y=.data$y)) + 
     ggplot2::geom_line(color="black",alpha=1) + 
     ggplot2::labs(x="age", y="Cumulative Hazard")
g

g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$x, y=.data$z)) + 
     ggplot2::geom_line(color="black",alpha=1) + 
     ggplot2::labs(x="age", y="Surviving Fraction")
g

