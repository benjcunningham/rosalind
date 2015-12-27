#' Finding a Protein Motif
#' 
#' To allow for the presence of its varying forms, a protein motif is
#' represented by a shorthand as follows: [XY] means "either X or Y" and
#' {X} means "any amino acid except X." For example, the N-glycosylation
#' motif is written as N{P}[ST]{P}.
#' 
#' You can see the complete description and features of a particular
#' protein by its access ID "uniprot_id" in the UniProt database, by
#' inserting the ID number into
#' 
#' http://www.uniprot.org/uniprot/uniprot_id
#' 
#' Alternatively, you can obtain a protein sequence in FASTA format by
#' following
#' 
#' http://www.uniprot.org/uniprot/uniprot_id.fasta
#' 
#' For example, the data for protein B5ZC00 can be found at
#' http://www.uniprot.org/uniprot/B5ZC00.
#' 
#' Given: At most 15 UniProt Protein Database access IDs.
#' 
#' Return: For each protein possessing the N-glycosylation motif, output
#' its given access ID followed by a list of locations in the protein
#' string where the motif can be found.

library(magrittr)
library(seqinr)

f <- 'data/rosalind_mprt.txt'
url <- 'http://www.uniprot.org/uniprot/'

ids <-
  readLines(f) %>%
  strsplit(split = '\n') %>%
  unlist()

df <- data.frame(ID = character(0), STRING = character(0))

for (i in 1:length(ids)) {
  
  df <-
    paste0(url, ids[i], '.fasta') %>%
    read.fasta(as.string = TRUE) %>%
    unlist() %>%
    {data.frame(
      ID = ids[i],
      STRING = toupper(.[[1]]),
      stringsAsFactors = FALSE)} %>%
    rbind(df, .)
  
}

mot <- gregexpr('(?=(N[^P][ST][^P]))', df$STRING, perl = TRUE)

lapply(seq_along(mot),function(i) paste(mot[[i]], collapse = ' ')) %>%
  unlist() %>%
  {ifelse(. != '-1',
          paste0(df$ID[seq_along(.)], '\n', ., '\n'), '-1')} %>%
  .[. != '-1'] %>%
  paste(collapse = '') %>%
  cat()
