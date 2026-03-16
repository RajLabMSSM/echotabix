# Run bgzip: conda

Support function for
[run_bgzip](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md).

## Usage

``` r
run_bgzip_conda(
  target_path,
  bgz_file = construct_tabix_path(target_path = target_path),
  chrom_col,
  start_col,
  end_col,
  comment_char = NULL,
  conda_env = "echoR_mini",
  verbose = TRUE
)
```

## Arguments

- target_path:

  Path to full GWAS/QTL summary statistics file.

- bgz_file:

  Path to resulting bgz-compressed file after `target_path` has been
  converted to tabix format.

- chrom_col:

  Name of the chromosome column in the `target_path` file.

- start_col:

  Name of the genomic start position column in the `target_path` file.

- end_col:

  Name of the genomic end position column in the `target_path` file.

- comment_char:

  A single character which, when present as the first character in a
  line, indicates that the line is to be omitted from indexing.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- verbose:

  Print messages.
