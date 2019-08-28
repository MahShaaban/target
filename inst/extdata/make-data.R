# load required libraries
library(tidyverse)
library(GenomicRanges)

# load and clean peaks data

read_tsv('https://raw.githubusercontent.com/suwangbio/BETA/master/BETA_test_data/3656_peaks.bed',
         col_names = c('seqname', 'start', 'end', 'peak_name', 'pval')) %>%
  write_tsv(gzfile('inst/extdata/3656_peaks.bed.gz'))

real_peaks <- read_tsv('inst/extdata/3656_peaks.bed.gz') %>%
  makeGRangesFromDataFrame(keep.extra.columns = TRUE)

usethis::use_data(real_peaks, overwrite = TRUE)

# load reference genome annotation in granges

read_tsv('https://raw.githubusercontent.com/suwangbio/BETA/master/BETA_1.0.7/BETA/references/hg19.refseq') %>%
  dplyr::select(name = `#name`, everything()) %>%
  write_tsv(gzfile('inst/extdata/hg19.refseq.gz'))

hg19 <- read_tsv('inst/extdata/hg19.refseq.gz') %>%
  makeGRangesFromDataFrame(keep.extra.columns = TRUE) %>%
  promoters(downstream = 100000, upstream = 100000)

# load the differential expression output in granges
read_tsv('https://raw.githubusercontent.com/suwangbio/BETA/master/BETA_test_data/AR_diff_expr.xls') %>%
  dplyr::select(ID = `#ID`, everything()) %>%
  mutate(ID = str_split(ID, '_at', simplify = TRUE)[, 1]) %>%
  write_tsv(gzfile('inst/extdata/AR_diff_expr.tsv.gz'))

real_transcripts <- read_tsv('inst/extdata/AR_diff_expr.tsv.gz') %>%
  inner_join(as_tibble(hg19), by = c('ID'='name')) %>%
  makeGRangesFromDataFrame(keep.extra.columns = TRUE)

usethis::use_data(real_transcripts, overwrite = TRUE)
