# Tabix-index a file: conda

Tabix-index a tabular summary statistics file.

## Usage

``` r
index_conda(
  bgz_file,
  chrom_i,
  start_i,
  end_i,
  comment_char,
  skip_lines = 0,
  force_new = FALSE,
  conda_env = "echoR_mini",
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

- force_new:

  Force the creation of a new bgzip file (*.bgz*) and a new tabix index
  file (*.tbi*).

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- verbose:

  Print messages.
