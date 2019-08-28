#' \code{target}: Predict Combined Function of Transcription Factors.
#'
#' Implement the BETA algorithm for infering direct target genes from
#' DNA-binding and perturbation expression data Wang et al. (2013)
#' <doi: 10.1038/nprot.2013.150>. Extend the algorithm to predict the combined
#' effect of two DNA-binding elements from comprable binding and expression
#' data.
#'
#' Predicting associated peaks and direct targets
#'
#' \code{\link{associated_peaks}}
#' \code{\link{direct_targets}}
#'
#' Plotting and testing predictions
#' \code{\link{plot_predictions}}
#' \code{\link{test_predictions}}
#'
#' Internal \code{target} functions:
#' \code{\link{merge_ranges}}
#' \code{\link{find_distance}}
#' \code{\link{score_peaks}}
#' \code{\link{score_regions}}
#' \code{\link{rank_product}}
#'
#' @docType package
#' @name target
NULL
