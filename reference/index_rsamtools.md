# Tabix-index a file: Rsamtools

Tabix-index a tabular summary statistics file.

## Usage

``` r
index_rsamtools(
  bgz_file,
  chrom_i,
  start_i,
  end_i,
  comment_char,
  skip = 0L,
  zeroBased = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- bgz_file:

  Path to a file that has been compressed with `bgzip` (e.g. via
  [run_bgzip](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)).

- comment_char:

  Comment character denoting which row contains the column names (e.g.
  "#CHR" or "SNP").

- verbose:

  Print messages.
