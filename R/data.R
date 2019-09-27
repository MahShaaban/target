#' AR peaks in LNCaP cell line
#'
#' Androgen recepor peaks from ChIP-Seq experiment in the LNCaP cell line.
#'
#' @format A \code{GRanges}
#'
#' @seealso \code{\link{real_transcripts}}
#' @seealso \code{\link{sim_peaks}}
#'
#' @source \url{https://github.com/suwangbio/BETA/blob/master/BETA_test_data/
#' 3656_peaks.bed}
#'
#' @examples
#' # load data
#' data('real_peaks')
#'
#' # locate the raw data
#' system.file('extdata', '3656_peaks.bed.gz', package = 'target')
#'
#' # locate the source code for preparing the data
#' system.file('extdata', 'make-data.R', package = 'target')
'real_peaks'

#' Differential expression of DHT treated LNCaP cell line
#'
#' The differential expression analysis output of LNCaP cell line treated with
#' DHT for 16 hours compared to non-treated cells. The REFSEQ transcript
#' identifiers were used to merge the data.frame with the transcript
#' coordinates from the hg19 reference genome.
#'
#' @format A \code{GRanges}
#'
#' @seealso \code{\link{real_peaks}}
#' @seealso \code{\link{sim_transcripts}}
#'
#' @source \url{https://github.com/suwangbio/BETA/blob/master/BETA_test_data/
#' AR_diff_expr.xls}
#' @source \url{https://github.com/suwangbio/BETA/blob/master/BETA_1.0.7/BETA/
#' references/hg19.refseq}
#'
#' @examples
#' # load data
#' data('real_transcripts')
#'
#' # locate the raw data
#' system.file('extdata', 'AR_diff_expr.tsv.gz', package = 'target')
#' system.file('extdata', 'hg19.refseq', package = 'target')
#'
#' # locate the source code for preparing the data
#' system.file('extdata', 'make-data.R', package = 'target')
'real_transcripts'

#' Simulated peaks
#'
#' is randomly generated peaks with random distances from the transcripts start
#' sites (TSS) of chromosome 1 of the mm10 mouse genome.
#'
#' @format A \code{GRanges}
#'
#' @seealso \code{\link{real_peaks}}
#' @seealso \code{\link{sim_transcripts}}
#'
#' @examples
#' # load data
#' data('sim_peaks')
#'
#' # locate the source code for preparing the data
#' system.file('extdata', 'make-data.R', package = 'target')
'sim_peaks'

#' Simulated transcripts
#' The transcripts chromosome 1 of the mm10 mouse genome with randomly singed
#' statistics assigned to each.
#'
#' @format A \code{GRanges}
#'
#' @seealso \code{\link{real_transcripts}}
#' @seealso \code{\link{sim_transcripts}}
#'
#' @examples
#' # load data
#' data('sim_transcripts')
#'
#' # locate the source code for preparing the data
#' system.file('extdata', 'make-data.R', package = 'target')
'sim_transcripts'
