# define helper function
# takes, txdb name and distance
# returns, data.frame of distances within transcripts of txdb
get_regions <- function(txdb, distance) {
  if (is.data.frame(txdb)) {
    trans <- makeGRangesFromDataFrame(txdb, keep.extra.columns = TRUE)
  } else {
    # get columns in txdb
    txdb_columns <- columns(txdb)

    # get transcripts
    trans <- transcripts(txdb, columns = txdb_columns)
  }

  # get regions
  as.data.frame(
    promoters(trans,
              downstream = distance * 1000,
              upstream = distance * 1000)
  )
}
