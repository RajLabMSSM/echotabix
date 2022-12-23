<img src='https://github.com/RajLabMSSM/echotabix/raw/main/inst/hex/hex.png' height='300'><br><br>
[![](https://img.shields.io/badge/devel%20version-0.99.9-black.svg)](https://github.com/RajLabMSSM/echotabix)
[![R build
status](https://github.com/RajLabMSSM/echotabix/workflows/rworkflows/badge.svg)](https://github.com/RajLabMSSM/echotabix/actions)
[![](https://img.shields.io/github/last-commit/RajLabMSSM/echotabix.svg)](https://github.com/RajLabMSSM/echotabix/commits/main)
[![](https://app.codecov.io/gh/RajLabMSSM/echotabix/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RajLabMSSM/echotabix)
[![License: GPL (\>=
3)](https://img.shields.io/badge/license-GPL%20(%3E=%203)-blue.svg)](https://cran.r-project.org/web/licenses/GPL%20(%3E=%203))
¶ <h5> ¶ Author: <i>Brian M. Schilder</i> ¶ </h5>
<h5> ¶ README updated: <i>Dec-22-2022</i> ¶ </h5>

## `echotabix`: Converts genomic data files to tabix format

(including liftover, sorting, compression, and indexing steps) with a
single function. Retrieves local or remote tabix VCF/tabular files with
helper functions for creating queries from subsets of genomic data (e.g
GWAS/QTL loci, genomic ranges). Multiple methods available at each step,
including wrapper functions for an up-to-date, `conda`-based
installation of `htslib` tools.

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
if(!require("remotes")) install.packages("remotes")

remotes::install_github("RajLabMSSM/echotabix")
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
