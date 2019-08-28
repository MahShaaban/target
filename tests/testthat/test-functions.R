context("test-functions")

library(IRanges)

test_that("merge_ranges works", {
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  mr <- mergeByOverlaps(query, subject)

  expect_s4_class(mr, 'DataFrame')
  expect_equal(nrow(mr), 3)
})

test_that("find_distance works", {
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  distance <- find_distance(query, subject)

  expect_true(is.numeric(distance))
  expect_equal(length(distance), 3)
})

test_that("score_peaks works", {
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  distance <- find_distance(query, subject)
  peak_score <- score_peaks(distance, 100000)

  expect_true(is.numeric(peak_score))
  expect_equal(length(peak_score), 3)
})

test_that("score_regions works", {
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  distance <- find_distance(query, subject)
  peak_score <- score_peaks(distance, 100000)

  regions <- c('region1', 'region1', 'region2')
  region_score <- score_regions(peak_score, regions)

  expect_true(is.numeric(region_score))
  expect_equal(length(region_score), 3)
})

test_that("rank_product works", {
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  distance <- find_distance(query, subject)
  peak_score <- score_peaks(distance, 100000)

  regions <- c('region1', 'region1', 'region2')
  region_score <- score_regions(peak_score, regions)

  region_stat <- c(30, 30, -40)
  rp <- rank_product(region_score, region_stat, regions)

  expect_s3_class(rp, 'data.frame')
  expect_equal(nrow(rp), 3)
  expect_equal(ncol(rp), 6)
})

test_that("rank_product works with multiple stats", {
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  distance <- find_distance(query, subject)
  peak_score <- score_peaks(distance, 100000)

  regions <- c('region1', 'region1', 'region2')
  region_score <- score_regions(peak_score, regions)

  region_stat <- list(c(30, 30, -40), c(30, 10, 40))

  rp <- rank_product(region_score, region_stat, regions)

  expect_s3_class(rp, 'data.frame')
  expect_equal(nrow(rp), 3)
  expect_equal(ncol(rp), 6)
})

test_that("associated_peaks works", {
  # load peaks and transcripts data
  data("real_peaks")
  data("real_transcripts")

  # associated peaks
  ap <- associated_peaks(real_peaks, real_transcripts, 'name2')

  expect_s4_class(ap, 'GenomicRanges')
  expect_true(all(c('distance', 'peak_score') %in% colnames(mcols(ap))))
})

test_that("direct_targets works", {
  # load peaks and transcripts data
  data("real_peaks")
  data("real_transcripts")

  # direct targets
  dt <- direct_targets(real_peaks, real_transcripts, 'name2', 't')

  expect_s4_class(dt, 'GenomicRanges')
  expect_true(all(c('score', 'stat', 'score_rank', 'stat_rank', 'rank') %in% colnames(mcols(dt))))
})

test_that("plot_predictions works", {
  # generate random values
  rn1 <- rnorm(100)
  rn2 <- rnorm(100, 2)
  e <- c(rn1, rn2)

  # generate grouping variable
  g <- rep(c('up', 'down'), times = c(length(rn1), length(rn2)))

  expect_null(
    plot_predictions(e,
                     group = g,
                     colors = c('red', 'green'),
                     labels = c('up', 'down'))
  )
})


test_that("test_predictions works", {
  # generate random values
  set.seed(123)
  rn1 <- rnorm(100)
  rn2 <- rnorm(100, 2)
  e <- c(rn1, rn2)

  # generate grouping variable
  g <- rep(c('up', 'down'), times = c(length(rn1), length(rn2)))

  ks <- ks.test(rn1, rn2)

  res <- test_predictions(e, g, compare = c('up', 'down'))

  expect_identical(ks$statistic, res$statistic)
  expect_identical(ks$p.value, res$p.value)
})
