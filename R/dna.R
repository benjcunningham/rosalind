library(magrittr)

f <- 'data/dna.txt'
sym <- c('A', 'C', 'G', 'T')

readLines(f) %>%
  strsplit(split = '') %>%
  table() %>%
  .[c('A', 'C', 'G', 'T')]
