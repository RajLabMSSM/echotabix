# [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) to VCF

Convert a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) to a
VCF file Used to be performed with [`bcftools convert`, but
`MungeSumstats` works much
better](https://github.com/RajLabMSSM/echolocatoR/blob/0ccf40d2f126f755074e731f82386e4e01d6f6bb/R/dataframe_2_vcf.R).
**WARNING:** This method only works for a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) with a
single sample. It cannot parse multiple pieces of information stored in
the same column.

## Usage

``` r
dt_to_vcf(
  dat,
  save_path = tempfile(fileext = "_converted.vcf"),
  tabix_index = FALSE,
  nThread = 1,
  ...
)
```

## Arguments

- dat:

  data.frame to convert to VCF file.

- save_path:

  File path to save formatted data. Defaults to
  `tempfile(fileext=".tsv.gz")`.

- tabix_index:

  Index the formatted summary statistics with
  [tabix](http://www.htslib.org/doc/tabix.md) for fast querying.

- nThread:

  The number of threads to use. Experiment to see what works best for
  your data on your hardware.

- ...:

  Additional arguments passed to
  [standardise_header](https://al-murphy.github.io/MungeSumstats/reference/standardise_header.html).

## Examples

``` r
if (FALSE) { # \dontrun{
save_path <- echotabix::dt_to_vcf(dat=echodata::BST1)
} # }
```
