# Tabix-index a file: seqminer

Tabix-index a tabular summary statistics file.

## Usage

``` r
index_seqminer(
  bgz_file,
  chrom_i,
  start_i,
  end_i,
  comment_char,
  skipLines = 0,
  verbose = TRUE
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
