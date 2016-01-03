#' Mortal Fibonacci Rabbits
#' 
#' Recall the definition of the Fibonacci numbers from “Rabbits and
#' Recurrence Relations”, which followed the recurrence relation
#' Fn=Fn−1+Fn−2 and assumed that each pair of rabbits reaches maturity
#' in one month and produces a single pair of offspring (one male, one
#' female) each subsequent month.
#' 
#' Our aim is to somehow modify this recurrence relation to achieve a
#' dynamic programming solution in the case that all rabbits die out
#' after a fixed number of months. See Figure 4 for a depiction of a
#' rabbit tree in which rabbits live for three months (meaning that they
#' reproduce only twice before dying).
#' 
#' Given: Positive integers n≤100 and m≤20.
#' 
#' Return: The total number of pairs of rabbits that will remain after
#' the n-th month if all rabbits live for m months.

library(gmp)
library(magrittr)

f <- 'data/rosalind_fibd.txt'

raw <-
  readLines(f) %>%
  strsplit(split = ' ') %>%
  unlist() %>%
  as.numeric()

n <- raw[1]
m <- raw[2]

a <- 0
b <- 1

for (i in 2:n) {
  
  a <-
    c(a, a[i - 1] + b[i - 1] - if(i > m) b[i - m] else 0) %>%
    as.bigz()
  
  b <- c(as.bigz(b), a[i - 1])
  
}

(a + b)[n] %>%
  as.character() %>%
  cat()
