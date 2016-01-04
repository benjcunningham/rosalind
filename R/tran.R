#' Transitions and Transversions
#' 
#' For DNA strings s1 and s2 having the same length, their transition /
#' transversion ratio R(s1,s2) is the ratio of the total number of
#' transitions to the total number of transversions, where symbol
#' substitutions are inferred from mismatched corresponding symbols as
#' when calculating Hamming distance (see “Counting Point Mutations”).
#' 
#' Given: Two DNA strings s1 and s2 of equal length (at most 1 kbp).
#' 
#' Return: The transition/transversion ratio R(s1,s2).

library(magrittr)
library(seqinr)

f <- 'data/rosalind_tran.txt'

dna <- 
  read.fasta(f, as.string = TRUE) %>%
  unlist() %>%
  {data.frame(
    ID = names(.),
    STRING = toupper(.),
    stringsAsFactors = FALSE)}

rownames(dna) <- NULL
names(dna) <- c('ID', 'STRING')

a <- unlist(strsplit(dna$STRING[1], split = NULL))
b <- unlist(strsplit(dna$STRING[2], split = NULL))

numer <- 0
denom <- 0

for (i in 1:length(a)) {
  
  if (a[i] != b[i]) {
    
    denom <- denom + 1
    numer <- numer + ifelse(all(c(a[i], b[i]) %in% c('A', 'G')) ||
                            all(c(a[i], b[i]) %in% c('C', 'T')), 1, 0)
      
  }
  
}

(numer / (denom - numer)) %>%
  cat()
