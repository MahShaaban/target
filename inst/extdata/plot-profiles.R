plot_profiles <- function(peaks, transcripts, n, change) {
  # set 2 by 3 plot matrix
  par(mfrow = c(2, 3))

  # define plot stats function
  plot_stats <- function(stat1, stat2, label, ...) {
    plot(stat1,
         stat2,
         xlab = 'Factor One',
         ylab = 'Factor Two',
         ...)
    abline(v = 0, h = 0, lty = 2, col = 'gray')

    return(NULL)
  }

  # scatter of statistics of two factor perturbations
  plot_stats(transcripts$stat1,
             transcripts$stat2,
             main = 'Before introducing change',
             label = 'A')

  # select transcripts to change and shift
  ap <- associated_peaks(peaks, transcripts, 'tx_id')
  nearby <- unique(ap$assigned_region[order(abs(ap$distance))[1:n]])
  ind <- transcripts$tx_id %in% nearby

  transcripts$stat1[ind] <- ifelse(transcripts$stat1[ind] > 0,
                                   transcripts$stat1[ind] * change[1],
                                   transcripts$stat1[ind])

  transcripts$stat2[ind] <- ifelse(transcripts$stat2[ind] > 0,
                                   transcripts$stat2[ind] * change[2],
                                   transcripts$stat2[ind])

  # scatter of statistics after adding positive change
  plot_stats(transcripts$stat1,
             transcripts$stat2,
             main = 'After introducing change',
             label = 'B')

  # associated peaks distance vs score
  ap <- associated_peaks(peaks, transcripts, 'tx_id', base = 100000)

  plot(ap$distance,
       ap$peak_score,
       xlab = 'Distance',
       ylab = 'Peak Score',
       main = 'Associated Peaks')
  abline(v = 0, lty = 2, col = 'gray')

  # predicted function of factor one
  dt1 <- direct_targets(peaks,
                        transcripts,
                        'tx_id',
                        'stat1',
                        base = 100000)

  # plot function of factor one
  g1 <- cut(dt1$stat1,
            breaks = quantile(dt1$stat1, c(0, .25, .75, 1)),
            labels = c('Down', 'None', 'Up'))

  plot_predictions(dt1$score_rank,
                   group = g1,
                   colors = c('darkgreen', 'gray', 'darkred'),
                   labels = c('Down', 'None', 'Up'),
                   xlab = 'Regulatory Potential',
                   ylab = 'ECDF',
                   main = 'Predicted function of factor one',
                   cex = .5)

  # predicted function of factor two
  dt2 <- direct_targets(peaks,
                        transcripts,
                        'tx_id',
                        'stat2',
                        base = 100000)

  # plot function of factor two
  g2 <- cut(dt1$stat2,
            breaks = quantile(dt2$stat2, c(0, .25, .75, 1)),
            labels = c('Down', 'None', 'Up'))

  plot_predictions(dt2$score_rank,
                   group = g2,
                   colors = c('darkgreen', 'gray', 'darkred'),
                   labels = c('Down', 'None', 'Up'),
                   xlab = 'Regulatory Potential',
                   ylab = 'ECDF',
                   main = 'Predicted function of factor two',
                   cex = .5)

  # predicted function of the combinded effect of the two factors
  dt <- direct_targets(peaks,
                       transcripts,
                       'tx_id',
                       c('stat1', 'stat2'),
                       base = 100000)

  # plot combined function of two factors
  g <- cut(dt$stat,
            breaks = quantile(dt$stat, c(0, .25, .75, 1)),
            labels = c('Competitive', 'None', 'Cooperative'))

  plot_predictions(dt$score_rank,
                   group = g,
                   colors = c('darkgreen', 'gray', 'darkred'),
                   labels = c('Competitive', 'None', 'Cooperative'),
                   xlab = 'Regulatory Potential',
                   ylab = 'ECDF',
                   main = 'Predicted function of factor two',
                   cex = .5)

  invisible(NULL)
}
