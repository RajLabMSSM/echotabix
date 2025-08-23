<img src='https://github.com/RajLabMSSM/echotabix/raw/main/inst/hex/hex.png' title='Hex sticker for echotabix' height='300'><br>
[![License: GPL (\>=
3)](https://img.shields.io/badge/license-GPL%20(%3E=%203)-blue.svg)](https://cran.r-project.org/web/licenses/GPL%20(%3E=%203))
[![](https://img.shields.io/badge/devel%20version-0.99.10-black.svg)](https://github.com/RajLabMSSM/echotabix)
[![](https://img.shields.io/github/languages/code-size/RajLabMSSM/echotabix.svg)](https://github.com/RajLabMSSM/echotabix)
[![](https://img.shields.io/github/last-commit/RajLabMSSM/echotabix.svg)](https://github.com/RajLabMSSM/echotabix/commits/main)
<br> [![R build
status](https://github.com/RajLabMSSM/echotabix/workflows/rworkflows/badge.svg)](https://github.com/RajLabMSSM/echotabix/actions)
[![](https://codecov.io/gh/RajLabMSSM/echotabix/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RajLabMSSM/echotabix)
<br>
<a href='https://app.codecov.io/gh/RajLabMSSM/echotabix/tree/main' target='_blank'><img src='https://codecov.io/gh/RajLabMSSM/echotabix/branch/main/graphs/icicle.svg' title='Codecov icicle graph' width='200' height='50' style='vertical-align: top;'></a>  
<h4>  
Authors: <i>Brian Schilder, Jack Humphrey, Towfique Raj</i>  
</h4>
<h5>  
README updated: <i>Aug-23-2025</i>  
</h5>

## `echotabix`: Converts genomic data files to tabix format (including liftover, sorting, compression, and indexing steps) with a single function. Retrieves local or remote tabix VCF/tabular files with helper functions for creating queries from subsets of genomic data (e.g GWAS/QTL loci, genomic ranges). Multiple methods available at each step, including wrapper functions for an up-to-date, `conda`-based installation of `htslib` tools.

This R package is part of the *echoverse* suite that supports
[`echolocatoR`](https://github.com/RajLabMSSM/echolocatoR): an automated
genomic fine-mapping pipeline.

If you use `echotabix`, please cite:

> Brian M Schilder, Jack Humphrey, Towfique Raj (2021) echolocatoR: an
> automated end-to-end statistical and functional genomic fine-mapping
> pipeline, *Bioinformatics*; btab658,
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

<hr>

## Contact

<a href="https://bschilder.github.io/BMSchilder/" target="_blank">Brian
M. Schilder, Bioinformatician II</a>  
<a href="https://rajlab.org" target="_blank">Raj Lab</a>  
<a href="https://icahn.mssm.edu/about/departments/neuroscience" target="_blank">Department
of Neuroscience, Icahn School of Medicine at Mount Sinai</a>
