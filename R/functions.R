#' Merge peaks and regions GRanges
#'
#' For internal use only and not intended to be used by the end users.
#'
#' @param peaks A GRanges object
#' @param regions A GRanges object
#'
#' @return A DataFrame
#'
#' @examples
#' library(IRanges)
#'
#' query <- IRanges(c(1, 4, 9), c(5, 7, 10))
#' subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
#' mergeByOverlaps(query, subject)
#'
#' @importFrom IRanges mergeByOverlaps
#'
#' @export
merge_ranges <- function(peaks, regions) {
  # get overlapping peaks on regions
  res <- mergeByOverlaps(peaks, regions)

  # return
  return(res)
}

#' Find the distance between peaks and regions
#'
#' For internal use only and not intended to be used by the end users.
#'
#' @inheritParams merge_ranges
#' @param how A character string, default 'center'
#'
#' @return A vector of integers
#'
#' @examples
#' library(IRanges)
#'
#' query <- IRanges(c(1, 4, 9), c(5, 7, 10))
#' subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
#' find_distance(query, subject)
#'
#' @importFrom BiocGenerics start end
#'
#' @export
find_distance <- function(peaks, regions, how = 'center') {
  # check valid arguments
  if (length(peaks) != length(regions)) {
    stop("The length of peaks and regions should be the same.")
  }

  if (!how %in% c('center', 'start', 'end')) {
    stop("how should be one of 'center', 'start' or 'end'.")
  }

  # calculate distance using how
  if (how == 'center') {
    # calculate peak and promoter center
    peak_center <- (start(peaks)+end(peaks))/2
    region_center <- round((start(regions)+end(regions))/2)

    # calculate the distances
    distance <- peak_center - region_center
  } else if (how == 'start') {
    # calculate the distances
    distance = start(peaks) - start(regions)
  } else if (how == 'end') {
    # calculate the distances
    distance = end(peaks) - end(regions)
  }

  # return
  return(distance)
}

#' Calculate peak scores
#'
#' For internal use only and not intended to be used by the end users.
#'
#' @param distance A vector of integers
#' @param base An integer to calculate distances relative to.
#'
#' @return A vector of integers
#'
#' @examples
#' library(IRanges)
#'
#' query <- IRanges(c(1, 4, 9), c(5, 7, 10))
#' subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
#' distance <- find_distance(query, subject)
#' score_peaks(distance, 100000)
#'
#' @export
score_peaks <- function(distance, base) {
  # check valid arguments
  if (!is.numeric(distance)) {
    stop("distance should be a numeric.")
  }

  if (!is.numeric(base) | length(base) != 1) {
    stop("base should be a numeric of length one.")
  }

  # calculate the peak score
  peak_score <- exp(-0.5 - (4 * abs(distance/base)))

  # return
  return(peak_score)
}

#' Calculate region scores
#'
#' For internal use only and not intended to be used by the end users.
#'
#' @param peak_score  A vector of integers
#' @param region_id  A vector of character
#'
#' @return A vector of numerics
#'
#' @examples
#' library(IRanges)
#'
#' query <- IRanges(c(1, 4, 9), c(5, 7, 10))
#' subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
#' distance <- find_distance(query, subject)
#' peak_score <- score_peaks(distance, 100000)
#' region_id <- c('region1', 'region1', 'region2')
#' region_score <- score_regions(peak_score, region_id)
#'
#' @importFrom stats aggregate
#'
#' @export
score_regions <- function(peak_score, region_id) {
  # check valid arguments
  if (!is.numeric(peak_score)) {
    stop("peak_score should be a numeric.")
  }

  if (length(peak_score) != length(region_id)) {
    stop("The length of peak_score and region_id should be the same.")
  }

  # make a data.frame of inputs
  dat <- data.frame(peak_score = peak_score,
                    region_id = region_id,
                    stringsAsFactors = FALSE)

  # calculate scores
  scores <- aggregate(dat$peak_score,
                      by = list(region_id = dat$region_id),
                      FUN = sum)

  # match scores in input order
  ind <- match(dat$region_id, scores$region_id)
  region_score <- scores$x[ind]

  # return
  return(region_score)
}

#' Calculate the regions rank products
#'
#' For internal use only and not intended to be used by the end users.
#'
#' @param region_score A vector of numerics
#' @param region_stat  A vector of numerics
#' @param region_id  A vector of characters
#'
#' @return  A vector of numerics
#'
#' @examples
#' library(IRanges)
#'
#' query <- IRanges(c(1, 4, 9), c(5, 7, 10))
#' subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
#' distance <- find_distance(query, subject)
#' peak_score <- score_peaks(distance, 100000)
#' region_id <- c('region1', 'region1', 'region2')
#' region_score <- score_regions(peak_score, region_id)
#' region_stat <- c(30, 30, -40)
#' rank_product(region_score, region_stat, region_id)
#'
#' @importFrom matrixStats rowProds
#'
#' @export
rank_product <- function(region_score, region_stat, region_id) {
  # check valid arguments
  if (!is.numeric(region_score)) {
    stop('region_score should be a numeric.')
  }

  if (is.list(region_stat)) {
    if (!all(unlist(lapply(region_stat, is.numeric)))) {
      stop('region_score should be a numeric or a list of numerics.')
    }

    if (!all(lengths(region_stat) == length(region_score))) {
      stop("The length of region_score and region_stat list items should be the same.")
    }
  } else {
    if (!is.numeric(region_stat)) {
      stop('region_score should be a numeric or a list of numerics.')
    }

    if (length(region_score) != length(region_stat)) {
      stop("The length of region_score and region_stat should be the same.")
    }
  }

  if (length(region_score) != length(region_id)) {
    stop("The length of region_score and region_id should be the same.")
  }

  # make a data.frames of inputs
  dat <- data.frame(region_id = region_id,
                    score = region_score,
                    stringsAsFactors = FALSE)

  # add the score rank
  dat$score_rank <- rank(-dat$score, ties.method = 'min')

  # calculate the stat product
  dat$stat <- rowProds(as.matrix(as.data.frame(region_stat)))

  # add the stat rank
  dat$stat_rank <- rank(-abs(dat$stat), ties.method = 'min')

  # calculate rank product
  n <- length(unique(dat$region_id))
  dat$rank <- (dat$score_rank/n) * (dat$stat_rank/n)

  # return dat
  return(dat)
}

#' Predicts associated peaks
#'
#' This function selects overlapping peaks and regions, calculates the distance
#' between them and score each peak.
#'
#' @inheritParams merge_ranges
#' @param regions_col A character string
#' @inheritParams score_peaks
#'
#' @return A GRanges object. A similar object to peaks with three added
#' metadata columns.
#'
#' @examples
#'\dontrun{
#' # load peaks and transcripts data
#' data("real_peaks")
#' data("real_transcripts")
#'
#' # associated peaks
#' ap <- associated_peaks(real_peaks, real_transcripts, 'name2')
#' }
#'
#' @importFrom GenomicRanges mcols
#'
#' @export
associated_peaks <- function(peaks, regions, regions_col, base = 100000) {
  # check valid arguments
  if (class(peaks) != 'GRanges') {
    stop("peaks should be a GRanges object.")
  }

  if (class(regions) != 'GRanges') {
    stop("regions should be a GRanges object.")
  }

  if (!regions_col %in% names(mcols(regions))) {
    stop('regions_col should be a column name in the metadata of regions.')
  }

  if (!is.numeric(base) | length(base) != 1) {
    stop("base should be a numeric of length one.")
  }

  # merge peaks and regions
  mr <- merge_ranges(peaks, regions)

  # find distance between centers of peaks and tss
  distance <- find_distance(mr$peaks, mr$regions)

  # score peaks by distance
  peak_score <- score_peaks(distance, base)

  # make associated peak ranges
  ap <- mr$peaks
  ap$assigned_region <- unlist(mr[regions_col], use.names = FALSE)
  ap$distance <- distance
  ap$peak_score <- peak_score

  # return
  return(ap)
}

#' Predicts direct targets
#'
#' This function selects overlapping peaks and regions, calculates the distance
#' between them, score each peak and region and calculate rank products of the
#' regions.
#'
#' @inheritParams merge_ranges
#' @inheritParams associated_peaks
#' @inheritParams score_peaks
#' @param stats_col A character string
#'
#' @return A GRanges object. A similar object to regions with several added
#' metadata columns.
#'
#' @examples
#'\dontrun{
#' # load peaks and transcripts data
#' data("real_peaks")
#' data("real_transcripts")
#'
#' # direct targets
#' dt <- direct_targets(real_peaks, real_transcripts, 'name2', 't')
#' }
#'
#' @importFrom GenomicRanges makeGRangesFromDataFrame mcols
#'
#' @export
direct_targets <- function(peaks, regions, regions_col, stats_col,
                           base = 100000) {
  # check valid arguments
  if (class(peaks) != 'GRanges') {
    stop("peaks should be a GRanges object.")
  }

  if (class(regions) != 'GRanges') {
    stop("regions should be a GRanges object.")
  }

  if (!regions_col %in% names(mcols(regions))) {
    stop('regions_col should be a column name in the metadata of regions.')
  }

  if(length(stats_col) != 1) {
    if (!all(stats_col %in% names(mcols(regions)))) {
      stop('regions_col should be column name/s in the metadata of regions.')
    }
  }

  if (!is.numeric(base) | length(base) != 1) {
    stop("base should be a numeric of length one.")
  }

  # merge peaks and regions
  mr <- merge_ranges(peaks, regions)

  # find distance between centers of peaks and tss
  distance <- find_distance(mr$peaks, mr$regions)

  # score peaks by distance
  peak_score <- score_peaks(distance, base)

  # score regions by the sum of peak scores
  regions <- unlist(mr[regions_col], use.names = FALSE)
  region_score <- score_regions(peak_score, regions)

  # calculate rank product
  stats <- unlist(mr[stats_col], use.names = FALSE)

  # make an input data.frame
  dat <- unique(
    data.frame(regions = regions,
               region_score = region_score,
               stats = stats,
               stringsAsFactors = FALSE)
  )

  # calculate rank product
  rp <- rank_product(dat$region_score, dat$stats, dat$regions)
  names(rp)[1] <- regions_col

  # make direct targets ranges
  dt <- unique(
    merge(as.data.frame(mr$regions), rp,
          by = regions_col,
          all.x = TRUE)
  )
  dt <- makeGRangesFromDataFrame(dt, keep.extra.columns = TRUE)

  # return
  return(dt)
}


#' Plot the ECDF of ranks by groups
#'
#' Plot the cumulative distribution function of choosen value (e.g. ranks) by
#' a factor of the same lenght, group. Each group is given a color and a label.
#'
#' @param rank A numeric vector
#' @param group A factor of length equal that of rank
#' @param labels A character vector of length equal the unique values in groups
#' @param colors A character vector of colors for each group
#' @param ... Other arguments passed to plot
#'
#' @return NULL.
#'
#' @examples
#' # generate random values
#' rn1 <- rnorm(100)
#' rn2 <- rnorm(100, 2)
#' e <- c(rn1, rn2)
#'
#' # generate grouping variable
#' g <- rep(c('up', 'down'), times = c(length(rn1), length(rn2)))
#'
#' plot_predictions(e,
#'                  group = g,
#'                  colors = c('red', 'green'),
#'                  labels = c('up', 'down'))
#'
#' @importFrom stats ecdf
#' @importFrom graphics plot points legend
#'
#' @export
plot_predictions <- function(rank, group, colors, labels, ...) {
  # check valid arguments
  if (length(rank) != length(group)) {
    stop("The length of rank and group should be the same.")
  }

  if (length(labels) != length(colors)) {
    stop("The labels of rank and colors should be the same.")
  }

  # split rank by group
  groups <- split(rank, group)

  # get x-axis limits
  xlim <- c(min(rank), max(rank))

  # plot empty graph
  plot(xlim,
       0:1,
       type = 'n',
       ...)

  # get length of the grou
  n <- length(groups)

  # loop over n groups, and
  for(i in 1:n) {
    # get values
    values <- groups[[i]]

    # get color
    color <- colors[i]

    # get values order
    ind <- order(values)

    # calculate ecdf
    ecdf_group <- ecdf(values)

    # plot points
    points(values[ind],
           ecdf_group(values)[ind],
           col = color,
           pch = 19)
  }

  # add legend
  legend('bottomright',
         legend = labels,
         col = colors,
         pch = 19)

  # return
  invisible(NULL)
}

#' Test the ECDF ranks of groups are from same distribution
#'
#' Test whether the cumulative distribution function of two groups are drawn
#' from the same distribution.
#'
#' @inheritParams plot_predictions
#' @param compare A character vector of length two
#' @param ... Other arguments passed to ks.test
#'
#' @return An htest object
#'
#' @examples
#' # generate random values
#' rn1 <- rnorm(100)
#' rn2 <- rnorm(100, 2)
#' e <- c(rn1, rn2)
#'
#' # generate grouping variable
#' g <- rep(c('up', 'down'), times = c(length(rn1), length(rn2)))
#'
#' # test
#' test_predictions(e,
#'                  group = g,
#'                  compare = c('up', 'down'))
#'
#' @importFrom stats ks.test
#'
#' @export
test_predictions <- function(rank, group, compare, ...) {
  # check valid arguments
  if (length(rank) != length(group)) {
    stop("The length of rank and group should be the same.")
  }

  if (length(compare) != 2) {
    stop('compare should be a vector of length two.')
  }

  if (!all(compare %in% unique(as.character(group)))) {
    stop('compare should be two of the unique values in group to compare.')
  }

  # get x and y
  groups <- split(rank, group)
  x <- groups[[compare[1]]]
  y <- groups[[compare[2]]]

  # test x y
  ks <- ks.test(x, y, ...)

  # return
  return(ks)
}

#' Run the shiny App
#'
#' @return NULL
#'
#' @importFrom shiny runApp
#'
#' @export
target_app <- function(){
  app_dir <- system.file('target-app', package = 'target')
  runApp(app_dir, display.mode = 'normal')
}
