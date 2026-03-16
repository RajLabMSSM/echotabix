# Tabix-index a file

Tabix-index a tabular summary statistics file.

## Usage

``` r
index(
  bgz_file,
  chrom_col,
  start_col,
  end_col = start_col,
  comment_char = NULL,
  force_new = TRUE,
  method = c("conda", "seqminer", "rsamtools", "variantannotation"),
  conda_env = "echoR_mini",
  verbose = TRUE
)
```

## Arguments

- bgz_file:

  Path to a file that has been compressed with `bgzip` (e.g. via
  [run_bgzip](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)).

- chrom_col:

  Name of the chromosome column in the `target_path` file.

- start_col:

  Name of the genomic start position column in the `target_path` file.

- end_col:

  Name of the genomic end position column in the `target_path` file.

- comment_char:

  Comment character denoting which row contains the column names (e.g.
  "#CHR" or "SNP").

- force_new:

  Force the creation of a new bgzip file (*.bgz*) and a new tabix index
  file (*.tbi*).

- method:

  Method to index tabix file with.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- verbose:

  Print messages.

## See also

Other tabix functions:
[`construct_tabix_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_tabix_path.md),
[`construct_vcf_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_vcf_path.md),
[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md),
[`query_table()`](https://rajlabmssm.github.io/echotabix/reference/query_table.md),
[`query_vcf()`](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md),
[`read_bgz()`](https://rajlabmssm.github.io/echotabix/reference/read_bgz.md),
[`run_bgzip()`](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
tmp <- tempfile(fileext = ".tsv.gz")
data.table::fwrite(dat, tmp, sep="\t")
bgz_file <- echotabix::run_bgzip(target_path = tmp,
                                 chrom_col = "CHR",
                                 start_col = "POS")
tbi_file <- echotabix::index(bgz_file = bgz_file,
                             chrom_col = "CHR",
                             start_col = "POS")
} # }
```
