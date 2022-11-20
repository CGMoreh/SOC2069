

setwd("/Users/nickriches/Documents/GitHub/AdventuresInR/docs")

plot(cars)

(1 + 2) / 22 # I am doing some calculations!!

# Here is a comment!!

sqrt(10)

seq(1, 10, 2)

plus_two <- function(x) return(x + 2)

plus_two(2)

install.packages("tidyverse")
library(tidyverse)

x <- 1

class(x)


x <- "poo"

class(x)

x <- c(1, 2,3, 4)

class(x)

mean(x)

x <- c(1, 2, "hello", "sausage")
class(x)

x <- as.list(c(1, 2, "hello", "sausage"))

class(x)[[1]][1]

