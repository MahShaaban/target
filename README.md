[![Travis build status](https://travis-ci.org/MahShaaban/target.svg?branch=master)](https://travis-ci.org/MahShaaban/target)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/MahShaaban/target?branch=master&svg=true)](https://ci.appveyor.com/project/MahShaaban/target)
[![Codecov test coverage](https://codecov.io/gh/MahShaaban/target/branch/master/graph/badge.svg)](https://codecov.io/gh/MahShaaban/target?branch=master)

# target

Predict Combined Function of Transcription Factors

Implement the BETA algorithm for infering direct target genes from DNA-binding
and perturbation expression data Wang et al. (2013). Extend the algorithm to 
predict the combined function of two DNA-binding elements from comprable 
binding and expression data.

# Installation

The `target` package can be installed from Bioconductor using `BiocManager`.

```{r install_biocmanager,eval=FALSE}
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("target")
```

## Getting started

The `target` package contains two simulated datasets. `sim_peaks` is random 
peaks with random distances from the transcripts of chromosome 1 of the mm10
mouse genome. `sim_transcripts` is the same transcripts with random singed 
statistics assigned to each. In the following two examples, we introduce
changes in these statistics to simulate conditions where two factors are 
working cooperatively or competitively on the same transcripts. 

```r
# load libraries
library(target)
```

```r
# load data
data("sim_peaks")
data("sim_transcripts")
```

To help visualize these cases, a plotting function `plot_profiles` was 
constructed to introduce the changes `change` in the statistics of the 
transcripts near the `n` number of peaks. The source code for the function is 
available in `inst/extdata/plot-profiles.R` which we source to use here. 
The output of the function is a series of plots to visualize the statistics of
the two factors before and after introducing the changes, the peaks distances 
and scores and the predicted functions of the factors individually and 
combined.

```r
# source the plotting function
source(system.file('extdata', 'plot-profiles.R', package = 'target'))
```

The first two inputs to the plotting function is the simulated peaks and 
transcripts. We chose to introduce positive changes to the statistics of the
transcripts with the top 5000 nearby peaks of the two factors.

```r
# simulate and plot cooperative factors
plot_profiles(sim_peaks,
              sim_transcripts,
              n = 5000,
              change = c(3, 3))
```

The changes introduced above are illustrated in the right upper quadrant of the
scatter plot. The predicted functions of the two factors are similar, as shown
by distribution function of the regulatory potential of their targets. 
Finally, when the targets are predicted based on the two statistics combined, 
the sign of the statistics product determines the direction of the factor
interactions. Here, more higher ranking transcripts had positive/red/
cooperative change associated with the two factors.

## References

Wang S, Sun H, Ma J, et al. Target analysis by integration of transcriptome and
ChIP-seq data with BETA. Nat Protoc. 2013;8(12):2502–2515. 
doi:10.1038/nprot.2013.150

# Citation

For citing the package use:

```{r citation, warning=FALSE}
# citing the package
citation("target")
