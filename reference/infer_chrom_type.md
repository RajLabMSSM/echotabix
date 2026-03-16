# Infer chromosome type

In a given vector or stored table, determine whether chromosome type is
of the format `"chr1" ` (returns `TRUE`) or `1` (returns `FALSE`).

## Usage

``` r
infer_chrom_type(chrom = NULL, path = NULL, chrom_col = "CHR", verbose = TRUE)
```

## Arguments

- chrom:

  A value (character, numeric, or integer) giving an example of the
  chromosome column.

- path:

  Path to stored file.

- chrom_col:

  The name of the chromosome column in the file.

- verbose:

  Print messages.

## Examples

``` r
echotabix::infer_chrom_type(chrom="chr1")
#> Chromosome format: chr1
#> [1] TRUE
echotabix::infer_chrom_type(chrom=1)
#> Chromosome format: 1
#> [1] FALSE
```
