#' Overlap Graphs
#' 
#' A graph whose nodes have all been labeled can be represented by an
#' adjacency list, in which each row of the list contains the two node
#' labels corresponding to a unique edge.
#' 
#' A directed graph (or digraph) is a graph containing directed edges,
#' each of which has an orientation. That is, a directed edge is
#' represented by an arrow instead of a line segment; the starting and
#' ending nodes of an edge form its tail and head, respectively. The
#' directed edge with tail v and head w is represented by (v,w) (but not
#' by (w,v)). A directed loop is a directed edge of the form (v,v).
#' 
#' For a collection of strings and a positive integer k, the overlap
#' graph for the strings is a directed graph Ok in which each string is
#' represented by a node, and string s is connected to string t with a
#' directed edge when there is a length k suffix of s that matches a
#' length k prefix of t, as long as s≠t; we demand s≠t to prevent
#' directed loops in the overlap graph (although directed cycles may be
#' present).
#' 
#' Given: A collection of DNA strings in FASTA format having total
#' length at most 10 kbp.
#' 
#' Return: The adjacency list corresponding to O3. You may return edges
#' in any order.

library(magrittr)
library(seqinr)
library(stringr)

f <- 'data/rosalind_grph.txt'

dna <-
  read.fasta(f, as.string = TRUE) %>%
  unlist() %>%
  {data.frame(
    ID = names(.),
    STRING = toupper(.),
    stringsAsFactors = FALSE)}

rownames(dna) <- NULL

dna$STR_BEG <- str_extract(dna$STRING, '.{3}$')
dna$STR_END <- str_extract(dna$STRING, '^.{3}')

out <- ''
for (i in 1:length(dna$ID)) {
  for (j in 1:length(dna$ID)) {
    if ((i != j) && (dna$STR_BEG[i] == dna$STR_END[j])) {
      out <- paste0(out, dna$ID[i], ' ', dna$ID[j], '\n')
    }
  }
}

cat(out)