# loading requried libraries
library(tidyverse)
library(GenomicRanges)
library(TxDb.Mmusculus.UCSC.mm10.knownGene)

library(BSgenome.Mmusculus.UCSC.mm10)

# transcript annotation (bed)
mm10_chr1_trans <- transcripts(TxDb.Mmusculus.UCSC.mm10.knownGene,
                               filter = list(CDSCHROM = 'chr1'),
                               columns = c('tx_id', 'gene_id'))
transcripts <- mm10_chr1_trans[!is.na(as.character(mm10_chr1_trans$gene_id))]
sim_transcripts <- promoters(transcripts, downstream = 100000, upstream = 100000)

# simulate stats
set.seed(123)
sim_transcripts$stat1 <- rnorm(length(sim_transcripts), 0, 10)
set.seed(321)
sim_transcripts$stat2 <- rnorm(length(sim_transcripts), 0, 10)

usethis::use_data(sim_transcripts, overwrite = TRUE)

# simulate peaks
set.seed(1234)
n = 10000
distances <- rnorm(n, 0, 5000)
widths <- sample(100:500, n, replace = TRUE)

sim_peaks <- tibble(seqname = 'chr1',
       start = sample(start(transcripts), n, replace = TRUE) + abs(distances)/2,
       end = start + widths,
       peak_name = paste0('peak_', 1:n)) %>%
  makeGRangesFromDataFrame(keep.extra.columns = TRUE)

usethis::use_data(sim_peaks, overwrite = TRUE)
