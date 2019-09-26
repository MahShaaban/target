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

# Citation

For citing the package use:

```{r citation, warning=FALSE}
# citing the package
citation("target")
