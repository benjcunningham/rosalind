#' Enumerating k-mers Lexicographically
#' 
#' Assume that an alphabet A has a predetermined order; that is, we write
#' the alphabet as a permutation A=(a1,a2,…,ak), where a1<a2<...<ak. For
#' instance, the English alphabet is organized as (A,B,...,Z).
#' 
#' Given two strings s and t having the same length n, we say that s
#' precedes t in the lexicographic order (and write s<Lext) if the first
#' symbol s[j] that doesn't match t[j] satisfies sj<tj in A.
#' 
#' Given: A collection of at most 10 symbols defining an ordered
#' alphabet, and a positive integer n (n≤10).
#' 
#' Return: All strings of length n that can be formed from the alphabet,
#' ordered lexicographically.

library(gtools)
library(magrittr)
library(plyr)

f <- 'data/rosalind_lexf.txt'

a <-
  readLines(f)[1] %>%
  strsplit(split = ' ') %>%
  unlist()

n <- as.numeric(readLines(f)[2])
  
perm <- permutations(length(a), n, repeats.allowed = TRUE)

mapvalues(perm, from = 1:length(a), to = a) %>%
  apply(1, function(x) paste(x, collapse = '')) %>%
  paste(collapse = '\n') %>%
  write(file = 'data/solved_lexf.txt')
