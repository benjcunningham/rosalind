#' RNA Splicing
#' 
#' After identifying the exons and introns of an RNA string, we only
#' need to delete the introns and concatenate the exons to form a new
#' string ready for translation.
#' 
#' Given: A DNA string s (of length at most 1 kbp) and a collection of
#' substrings of s acting as introns. All strings are given in FASTA
#' format.
#' 
#' Return: A protein string resulting from transcribing and translating
#' the exons of s. (Note: Only one solution will exist for the dataset
#' provided.)

library(magrittr)
library(plyr)
library(seqinr)

f <- 'data/rosalind_splc.txt'

cod <- read.csv('data/rna_codon_table.csv', stringsAsFactors = FALSE)

dna <- 
  read.fasta(f, as.string = TRUE) %>%
  unlist() %>%
  {data.frame(
    ID = names(.),
    STRING = toupper(.),
    stringsAsFactors = FALSE)}

rownames(dna) <- NULL
names(dna) <- c('ID', 'STRING')

for (i in 2:nrow(dna)) {
  dna$STRING[1] <- gsub(dna$STRING[i], '', dna$STRING[1])
}

dna$STRING[1] %>%
  gsub('T', 'U', .) %>%
  gsub('(.{3})', '\\1 ', .) %>%
  strsplit(split = ' ') %>%
  unlist() %>%
  mapvalues(from = cod$CODON, to = cod$ACID, warn_missing = FALSE) %>%
  na.omit() %>%
  paste(collapse = '') %>%
  cat()
