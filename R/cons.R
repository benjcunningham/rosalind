library(magrittr)
library(seqinr)

f <- 'data/rosalind_cons.txt'

prof_matrix <- function(m, i) {
  paste(m[i, ], collapse = ' ') %>%
    paste0(rownames(m)[i], ': ', .)
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

m <-
  dna$STRING %>%
  lapply(function(x) unlist(strsplit(x, split = NULL))) %>%
  do.call(rbind, .) %>%
  apply(2, function(x) table(factor(x, levels = c('A','C','G','T'))))

con <-
  lapply(1:ncol(m), function(i) which.max(m[,i])) %>%
  unlist() %>%
  names() %>%
  paste(collapse = '')

pmat <-
  lapply(1:nrow(m), function(i) prof_matrix(m, i)) %>%
  paste(collapse = '\n')

paste(con, pmat, sep = '\n') %>%
  cat()
