# Run bgzip: Rsamtools

Support function for
[run_bgzip](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md).

## Usage

``` r
run_bgzip_rsamtools(target_path, bgz_file, force_new = TRUE, verbose = TRUE)
```

## Arguments

- target_path:

  Path to full GWAS/QTL summary statistics file.

- bgz_file:

  Path to resulting bgz-compressed file after `target_path` has been
  converted to tabix format.

- force_new:

  Force the creation of a new bgzip file (*.bgz*) and a new tabix index
  file (*.tbi*).

- verbose:

  Print messages.
