![](https://github.com/RajLabMSSM/echotabix/raw/main/inst/hex/hex.png "Hex sticker for echotabix")\
[![License: GPL (\>=
3)](https://img.shields.io/badge/license-GPL%20(%3E=%203)-blue.svg)](https://cran.r-project.org/web/licenses/GPL%20(%3E=%203))
[![](https://img.shields.io/badge/devel%20version-1.0.1-black.svg)](https://github.com/RajLabMSSM/echotabix)
[![](https://img.shields.io/github/languages/code-size/RajLabMSSM/echotabix.svg)](https://github.com/RajLabMSSM/echotabix)
[![](https://img.shields.io/github/last-commit/RajLabMSSM/echotabix.svg)](https://github.com/RajLabMSSM/echotabix/commits/main)\
[![R build
status](https://github.com/RajLabMSSM/echotabix/workflows/rworkflows/badge.svg)](https://github.com/RajLabMSSM/echotabix/actions)
[![](https://codecov.io/gh/RajLabMSSM/echotabix/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RajLabMSSM/echotabix)\
[![](https://codecov.io/gh/RajLabMSSM/echotabix/branch/main/graphs/icicle.svg "Codecov icicle graph")](https://app.codecov.io/gh/RajLabMSSM/echotabix/tree/main)\

#### \
Authors: *Brian Schilder, Jack Humphrey, Towfique Raj*\

##### \
README updated: *Mar-16-2026*\

## `echotabix`: Converts genomic data files to tabix format (including liftover, sorting, compression, and indexing steps) with a single function. Retrieves local or remote tabix VCF/tabular files with helper functions for creating queries from subsets of genomic data (e.g GWAS/QTL loci, genomic ranges). Multiple methods available at each step, including wrapper functions for an up-to-date, `conda`-based installation of `htslib` tools.

This R package is part of the *echoverse* suite that supports
[`echolocatoR`](https://github.com/RajLabMSSM/echolocatoR): an automated
genomic fine-mapping pipeline.

If you use `echotabix`, please cite:

> Brian M Schilder, Jack Humphrey, Towfique Raj (2021). echolocatoR: an
> automated end-to-end statistical and functional genomic fine-mapping
> pipeline. Bioinformatics, btab658.
> <https://doi.org/10.1093/bioinformatics/btab658>

## Installation

``` r

if(!require("BiocManager")) install.packages("BiocManager")

BiocManager::install("RajLabMSSM/echotabix")
library(echotabix)
```

## Documentation

### [Website](https://rajlabmssm.github.io/echotabix)

### [Getting started](https://rajlabmssm.github.io/echotabix/articles/echotabix)

------------------------------------------------------------------------

## Contact

[Brian M. Schilder, Bioinformatician
II](https://bschilder.github.io/BMSchilder/)\
[Raj Lab](https://rajlab.org)\
[Department of Neuroscience, Icahn School of Medicine at Mount
Sinai](https://icahn.mssm.edu/about/departments-offices/neuroscience)
