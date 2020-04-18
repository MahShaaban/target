library(testthat)
source('modules.R')
source('demo.R')

test_that("run_target and make_predictions", {
    peaks <- read_tsv(demo$peaks$file)
    expression <- read_tsv(demo$expression$file)
    genome <- read_tsv(demo$genome$file)
    
    ap <- run_target(
        return = 'associated_peaks',
        peaks,
        expression,
        genome,
        demo$expression$name_column,
        demo$genome$name_column,
        distance = demo$peaks$distance
    )
    expect_s3_class(ap, 'data.frame')
    expect_true(nrow(ap) > 0)
    
    dt <- run_target(
        return = 'direct_targets',
        peaks,
        expression,
        genome,
        demo$expression$name_column,
        demo$genome$name_column,
        factor_num = demo$expression$factor_number,
        stat1 = demo$expression$stat_column1,
        stat2 = demo$expression$stat_column2,
        distance = demo$peaks$distance
    )
    expect_s3_class(dt, 'data.frame')
    expect_true(nrow(dt) > 0)
    
    fl <- tempfile(fileext = 'png')
    png(fl)
    expect_null(make_prediction(dt, params = demo_plot))
    dev.off()
    
    tst <- make_prediction(dt, params = demo_test, return = 'test')
    
    expect_s3_class(tst, 'data.frame')
    expect_true(nrow(tst) > 0)
})

test_that("make_groups_quantiles", {
    grouping <- seq(1:9)
    labels <- c('low', 'mod', 'high')
    
    expect_error(make_groups_quantiles(grouping, c(.3, .7, .8), labels))
    expect_length(make_groups_quantiles(grouping, c(.3, .7), labels), 9)
    expect_length(table(make_groups_quantiles(grouping, c(.3, .7), labels)), 3)
    expect_true(all(table(make_groups_quantiles(grouping, c(.3, .7), labels)) == 3))
})

test_that("make_groups_top", {
    grouping <- seq(1:6)
    labels <- c('low', 'mod', 'high')
    
    expect_length(make_groups_top(grouping, c(-2, 2), labels), 6)
    expect_error(make_groups_top(grouping, c(-2, 2, 3), labels))
    expect_length(table(make_groups_top(grouping, c(-2, 2), labels)), 3)
    expect_true(all(table(make_groups_top(grouping, c(-2, 2), labels)) == 2))
    
    grouping <- seq(1:6)
    labels <- c('low', 'mod', 'high')
    
    expect_length(make_groups_top(grouping, c(-3, 3), labels), 6)
    expect_length(table(make_groups_top(grouping, c(-3, 3), labels)), 2)
    expect_true(all(table(make_groups_top(grouping, c(-3, 3), labels)) == 3))
})

test_that("make_groups_values", {
    grouping <- seq(1:9)
    labels <- c('low', 'mod', 'high')
    make_groups_values(grouping, c(3, 6), labels)
    
    expect_length(make_groups_values(grouping, c(3, 6), labels), 9)
    expect_error(make_groups_values(grouping, c(3, 6, 7), labels))
    expect_length(table(make_groups_values(grouping, c(3, 6), labels)), 3)
    expect_true(all(table(make_groups_values(grouping, c(3, 6), labels)) == 3))
})

test_that("load_genome", {
    library(TxDb.Mmusculus.UCSC.mm9.knownGene)
    
    expect_s3_class(load_genome('mm9'), 'data.frame')
    expect_true(all(c('GENEID', 'TXID') %in% names(load_genome('mm10'))))
})

test_that("unpack", {
    vec <- c('one','two','three')
    s <- paste(vec, collapse = ',')
    
    expect_identical(unpack(s), vec)
    
    vec <- c(-1,0,1)
    s <- paste(vec, collapse = ',')
    
    expect_identical(unpack(s, as = 'numeric'), vec)
})
