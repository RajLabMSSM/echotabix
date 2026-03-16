# Rsamtools warning

Warn users about outdated version of `htslib` used by older versions of
the Rhtslib R package.

## Usage

``` r
rhtslib_warning(
  rhtslib_pkgs = c("variantannotation", "rsamtools", "rtracklayer"),
  method = NULL,
  verbose = TRUE
)
```

## Source

[Rsamtools/Rhtslib
updates](https://github.com/Bioconductor/Rsamtools/issues/33#)

[Rhtslib\<1.99.2 (which Rsamtools and seqminer depend on for tabix) is
very out of date (uses htslib 1.7 vs.
1.15)](https://github.com/Bioconductor/Rhtslib/issues/4).

[Rsamtools: GitHub](https://github.com/Bioconductor/Rsamtools)

Rsamtools: Bioconductor
([doi:10.18129/B9.bioc.Rsamtools](https://doi.org/10.18129/B9.bioc.Rsamtools)
)

## Arguments

- rhtslib_pkgs:

  List of R packages that depend on `Rhtslib`.

- method:

  Method requested.

- verbose:

  Print messages.

## Value

Whether the Bioc version is invalid for this function.
