#' Counting Point Mutations
#' 
#' Given two strings s and t of equal length, the Hamming distance
#' between s and t, denoted dH(s,t), is the number of corresponding
#' symbols that differ in s and t. See Figure 2.
#' 
#' Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
#' 
#' Return: The Hamming distance dH(s,t).

library(magrittr)

f <- 'data/rosalind_hamm.txt'

read.csv(f, stringsAsFactors = FALSE, header = FALSE) %>%
  .[[1]] %>%
  strsplit(split = NULL) %>%
  {.[[1]] != .[[2]]} %>%
  sum()
