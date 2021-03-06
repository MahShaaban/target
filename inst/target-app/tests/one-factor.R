app <- ShinyDriver$new("../", loadTimeout = 100000)
app$snapshotInit("one-factor")

app$uploadFile(peaks = system.file('extdata', '3656_peaks.bed.gz', package = 'target'))
app$uploadFile(expression = system.file('extdata', 'AR_diff_expr.tsv.gz', package = 'target'))
app$setInputs(region_id = "ID")
app$setInputs(type = "One")
app$setInputs(stat_id = "t")
app$uploadFile(genome = system.file('extdata', 'hg19.refseq.gz', package = 'target'))
app$setInputs(genome_id_col = "name")
app$snapshot()
