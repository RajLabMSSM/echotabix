# Query a tabix file

Query by genomic coordinates.

## Usage

``` r
query_table(
  target_path,
  target_index = paste0(target_path, ".tbi"),
  query_granges,
  query_method = c("rsamtools", "seqminer", "conda"),
  local = NULL,
  overlapping_only = FALSE,
  query_save = TRUE,
  save_path = tempfile(fileext = "tsv.gz"),
  cleanup_tbi = TRUE,
  conda_env = "echoR_mini",
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- target_path:

  Path to tabix file.

- target_index:

  Tabix index file for `target_path`.

- query_granges:

  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object to be used for querying the `target_path` file. Alternatively,
  can be variant-level summary statistics to be converted into a
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object by
  [construct_query](https://rajlabmssm.github.io/echotabix/reference/construct_query.md).

- query_method:

  Method used for querying. See
  [query](https://rajlabmssm.github.io/echotabix/reference/query.md) for
  available options.

- local:

  Whether `target_path` is stored locally or on a remote server/website.
  By default (`NULL`) will infer local status and use the appropriate
  `query_method`.

- overlapping_only:

  Remove variants that do not overlap with the positions in `query_dat`.

- query_save:

  Whether to save the queried data subset.

- save_path:

  File path to save query subset to (as table).

- cleanup_tbi:

  Remove local copies of tabix index file (*.tbi*) after completing
  queries.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- nThread:

  Number of threads to use.

- verbose:

  Print messages.

## Value

`data.table` with the queried subset of genomic data.

## See also

Other tabix functions:
[`construct_tabix_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_tabix_path.md),
[`construct_vcf_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_vcf_path.md),
[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md),
[`index`](https://rajlabmssm.github.io/echotabix/reference/index.md),
[`query_vcf()`](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md),
[`read_bgz()`](https://rajlabmssm.github.io/echotabix/reference/read_bgz.md),
[`run_bgzip()`](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)

## Examples

``` r
if (FALSE) { # \dontrun{
query_dat <- echodata::BST1

#### local ####
target_path <- echodata::example_fullSS()
tabix_files <- echotabix::convert(target_path = target_path,
                                  start_col = "BP")
query_res <- echotabix::query_table(
    target_path = tabix_files$path,
    query_dat = query_dat)

#### remote ####
target_path <- file.path(
    "https://egg2.wustl.edu/roadmap/data/byFileType",
    "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
    "E099_15_coreMarks_dense.bed.bgz"
)
query_res2 <- echotabix::query_table(
    target_path = target_path,
    query_granges = query_dat)
} # }
```
