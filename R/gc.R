#' Computing GC Content
#' 
#' The GC-content of a DNA string is given by the percentage of symbols
#' in the string that are 'C' or 'G'. For example, the GC-content of
#' "AGCTATAG" is 37.5%. Note that the reverse complement of any DNA
#' string has the same GC-content.
#' 
#' DNA strings must be labeled when they are consolidated into a
#' database. A commonly used method of string labeling is called FASTA
#' format. In this format, the string is introduced by a line that
#' begins with '>', followed by some labeling information. Subsequent
#' lines contain the string itself; the first line to begin with '>'
#' indicates the label of the next string.
#' 
#' In Rosalind's implementation, a string in FASTA format will be
#' labeled by the ID "Rosalind_xxxx", where "xxxx" denotes a four-digit
#' code between 0000 and 9999.
#' 
#' Given: At most 10 DNA strings in FASTA format (of length at most 1
#' kbp each).
#' 
#' Return: The ID of the string having the highest GC-content, followed
#' by the GC-content of that string. Rosalind allows for a default error
#' of 0.001 in all decimal answers unless otherwise stated; please see
#' the note on absolute error below.

library(magrittr)
library(seqinr)

f <- 'data/rosalind_gc.txt'

gc_content <- function(x) {
  strsplit(x, split = NULL) %>%
    {unlist(.) %in% c('C', 'G')} %>%
    {sum(.) / length(.) * 100}
}

dna <- 
  read.fasta(f, as.string = TRUE) %>%
  unlist() %>%
  {data.frame(
    ID = names(.),
    STRING = toupper(.),
    stringsAsFactors = FALSE)}

rownames(dna) <- NULL
names(dna) <- c('ID', 'STRING')

gcc <-
  dna$STRING %>%
  lapply(function(x) gc_content(x)) %>%
  unlist()

dna$ID[which.max(gcc)] %>%
  paste(max(gcc), sep = '\n') %>%
  cat()
