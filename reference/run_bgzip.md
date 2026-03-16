# Run bgzip

Compress a file using bgzip.

## Usage

``` r
run_bgzip(
  target_path,
  chrom_col,
  start_col,
  end_col = start_col,
  comment_char = NULL,
  bgz_file = construct_tabix_path(target_path = target_path),
  sort_rows = TRUE,
  force_new = TRUE,
  method = c("rsamtools", "conda"),
  conda_env = "echoR_mini",
  validate = TRUE,
  verbose = TRUE
)
```

## Arguments

- target_path:

  Path to full GWAS/QTL summary statistics file.

- chrom_col:

  Name of the chromosome column in the `target_path` file.

- start_col:

  Name of the genomic start position column in the `target_path` file.

- end_col:

  Name of the genomic end position column in the `target_path` file.

- comment_char:

  A single character which, when present as the first character in a
  line, indicates that the line is to be omitted from indexing.

- bgz_file:

  Path to resulting bgz-compressed file after `target_path` has been
  converted to tabix format.

- sort_rows:

  Sort rows by genomic coordinates.

- force_new:

  Force the creation of a new bgzip file (*.bgz*) and a new tabix index
  file (*.tbi*).

- method:

  Method used to bgzip-compress the `target_path` file.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- validate:

  Check that the bgzip file exists and can be read in as a
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

- verbose:

  Print messages.

## See also

Other tabix functions:
[`construct_tabix_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_tabix_path.md),
[`construct_vcf_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_vcf_path.md),
[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md),
[`index`](https://rajlabmssm.github.io/echotabix/reference/index.md),
[`query_table()`](https://rajlabmssm.github.io/echotabix/reference/query_table.md),
[`query_vcf()`](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md),
[`read_bgz()`](https://rajlabmssm.github.io/echotabix/reference/read_bgz.md)

## Examples

``` r
if (FALSE) { # \dontrun{
#### Example with full data ####
# tmp <- echodata::example_fullSS()
#### Example with single locus ####
dat <- echodata::BST1
tmp <- tempfile()
data.table::fwrite(dat, tmp)

bgz_file <-  echotabix::run_bgzip(target_path=tmp,
                                 chrom_col="CHR",
                                 start_col="BP")
} # }
```
