# Query tabix

Query a tabix table or VCF.

## Usage

``` r
query(
  target_path,
  target_index = paste0(target_path, ".tbi"),
  target_format = NULL,
  query_granges,
  samples = character(),
  query_save = TRUE,
  query_save_path = tempfile(fileext = ".gz"),
  target_genome = "GRCh37",
  query_genome = "GRCh37",
  query_method = c("rsamtools", "variantannotation", "conda", "seqminer"),
  conda_env = "echoR_mini",
  query_force_new = FALSE,
  as_datatable = TRUE,
  overlapping_only = FALSE,
  cleanup_tbi = TRUE,
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- target_path:

  Path to full GWAS/QTL summary statistics file.

- target_index:

  Tabix index file for `target_path`.

- target_format:

  Format of the `target_path` file: "vcf" or "table".

- query_granges:

  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object to be used for querying the `target_path` file. Alternatively,
  can be variant-level summary statistics to be converted into a
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object by
  [construct_query](https://rajlabmssm.github.io/echotabix/reference/construct_query.md).

- samples:

  \[Optional\] Sample names to subset the VCF by. If this option is
  used, the
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object will be converted to a
  [ScanVcfParam](https://rdrr.io/pkg/VariantAnnotation/man/ScanVcfParam-class.html)
  for usage by
  [readVcf](https://rdrr.io/pkg/VariantAnnotation/man/readVcf-methods.html).

- query_save:

  Whether to save the queried data subset.

- query_save_path:

  Path to save retrieved query subset to.

- target_genome:

  Genome build of the VCF file.

- query_genome:

  Genome build that the `query_granges` is aligned to.

- query_method:

  Method used for querying. See query for available options.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- query_force_new:

  If the query subset (`query_save_path`) already exists, set
  `query_force_new=TRUE` to retrieve a new query subset.

- as_datatable:

  Return the VCF subset file as a
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  (using
  [vcf_to_dt](https://rajlabmssm.github.io/echotabix/reference/vcf_to_dt.md)).
  If `save_path=TRUE` the file will still be saved as a bgzip-compressed
  VCF file.

- overlapping_only:

  Remove variants that do not overlap with the positions in `query_dat`.

- cleanup_tbi:

  Remove local copies of tabix index file (*.tbi*) after completing
  queries.

- nThread:

  Number of threads to use.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
query_dat <- echodata::BST1

#### local ####
target_path <- echodata::example_fullSS()
tabix_files <- echotabix::convert(target_path = target_path,
                                  start_col = "BP")
query_res <- echotabix::query(
    target_path = tabix_files$path,
    query_granges = query_dat)
} # }
```
