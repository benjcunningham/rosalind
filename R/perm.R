#' Enumerating Gene Orders
#' 
#' A permutation of length n is an ordering of the positive integers
#' {1,2,…,n}. For example, π=(5,3,2,1,4) is a permutation of length 5.
#' 
#' Given: A positive integer n≤7.
#' 
#' Return: The total number of permutations of length n, followed by a
#' list of all such permutations (in any order).

library(gtools)
library(magrittr)

f <- 'data/rosalind_perm.txt'

n <-
  readLines(f) %>%
  as.numeric()

perm <- permutations(n, n, 1:n)

perm %>%
  apply(., 1, paste, collapse = ' ') %>%
  paste(collapse = '\n') %>%
  paste(nrow(perm), ., sep = '\n') %>%
  write(file = 'data/solved_perm.txt')
