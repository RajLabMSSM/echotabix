<!-- badges: start -->
<!-- badger::badge_codecov() -->
<!-- badger::badge_last_commit()  -->
<!-- badger::badge_license() -->

[![](https://codecov.io/gh/RajLabMSSM/echotabix/branch/main/graph/badge.svg)](https://codecov.io/gh/RajLabMSSM/echotabix)
[![R-CMD-check](https://github.com/RajLabMSSM/echotabix/workflows/R-full/badge.svg)](https://github.com/RajLabMSSM/echotabix/actions)
[![](https://img.shields.io/github/last-commit/RajLabMSSM/echotabix.svg)](https://github.com/RajLabMSSM/echotabix/commits/main)
<!-- badges: end -->

# echoverse: echotabix

## Tabix indexing and querying.

[`Tabix`](http://www.htslib.org/doc/tabix.html) is an extremely
efficient CLI tool for querying large genomic data. However, using it
reliably within R is still a challenge. `echotabix` efficiently creates
and queries `Tabix` files across a variety of scenarios:

-   Checks whether files are already `Tabix`-ready (indexed), and if
    not, converts them.
-   Queries files in tabular (e.g. *.csv*, *.tsv*, *.bed*) or VCF
    format.
-   Queries files stored locally or on the remote server.
-   Reports the size of the returned query.

``` r
if(!"remotes" %in% rownames(installed.packages())){install.packages("remotes")}

remotes::install_github("RajLabMSSM/echotabix")
library(echotabix)
```

## [Documentation website](https://rajlabmssm.github.io/echotabix/)

## [Vignette](https://rajlabmssm.github.io/echotabix/articles/echotabix)

<hr>

## Creator

<a href="https://bschilder.github.io/BMSchilder/" target="_blank">Brian
M. Schilder, Bioinformatician II</a>  
<a href="https://rajlab.org" target="_blank">Raj Lab</a>  
<a href="https://icahn.mssm.edu/about/departments/neuroscience" target="_blank">Department
of Neuroscience, Icahn School of Medicine at Mount Sinai</a>
