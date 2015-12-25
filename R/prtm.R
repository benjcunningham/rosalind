#' Calculating Protein Mass
#' 
#' In a weighted alphabet, every symbol is assigned a positive real
#' number called a weight. A string formed from a weighted alphabet is
#' called a weighted string, and its weight is equal to the sum of the
#' weights of its symbols.
#' 
#' The standard weight assigned to each member of the 20-symbol amino
#' acid alphabet is the monoisotopic mass of the corresponding amino
#' acid.
#' 
#' Given: A protein string P of length at most 1000 aa.
#' 
#' Return: The total weight of P. Consult the monoisotopic mass table.

library(magrittr)
library(plyr)

f <- 'data/rosalind_prtm.txt'

mon <- read.csv('data/monoisotopic_mass_table.csv',
                stringsAsFactors = FALSE)

readLines(f) %>%
  strsplit(split = NULL) %>%
  unlist() %>%
  mapvalues(from = mon$ACID, to = mon$MON.MASS,
            warn_missing = FALSE) %>%
  as.numeric() %>%
  sum() %>%
  format(nsmall = 3) %>%
  cat()
