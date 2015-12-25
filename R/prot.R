#' Translating RNA into Protein
#' 
#' The 20 commonly occurring amino acids are abbreviated by using 20
#' letters from the English alphabet (all letters except for B, J, O, U,
#' X, and Z). Protein strings are constructed from these 20 symbols.
#' Henceforth, the term genetic string will incorporate protein strings
#' along with DNA strings and RNA strings.
#' 
#' The RNA codon table dictates the details regarding the encoding of
#' specific codons into the amino acid alphabet.
#' 
#' Given: An RNA string s corresponding to a strand of mRNA (of length
#' at most 10 kbp).
#' 
#' Return: The protein string encoded by s.

library(magrittr)
library(plyr)

f <- 'data/rosalind_prot.txt'

cod <- read.csv('data/rna_codon_table.csv', stringsAsFactors = FALSE)

readLines(f) %>%
  gsub('(.{3})', '\\1 ', .) %>%
  strsplit(split = ' ') %>%
  unlist() %>%
  mapvalues(from = cod$CODON, to = cod$ACID,
            warn_missing = FALSE) %>%
  na.omit() %>%
  paste(collapse = '') %>%
  cat()