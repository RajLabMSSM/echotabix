# Convert and query

If it is not tabix format already (determined by checking for a `.tbi`
file of the same name in the same directory), the full summary
statistics file is converted into tabix format for super fast querying.
A query is then made using the min/max genomic positions to extract a
locus-specific summary stats file.

## Usage

``` r
convert_and_query(
  target_path,
  target_index = paste0(target_path, ".tbi"),
  target_format = NULL,
  study_dir = NULL,
  target_chrom_col = "CHR",
  target_start_col = "POS",
  target_end_col = target_start_col,
  query_granges,
  samples = character(),
  query_save = TRUE,
  query_save_path = tempfile(fileext = ".gz"),
  target_genome = "GRCh37",
  query_genome = "GRCh37",
  convert_methods = list(sort_coordinates = "bash", run_bgzip = "Rsamtools", index =
    "Rsamtools"),
  query_method = c("rsamtools", "seqminer", "conda"),
  conda_env = "echoR_mini",
  convert_force_new = FALSE,
  query_force_new = FALSE,
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

- study_dir:

  Path to study folder.

- target_chrom_col:

  Name of the chromosome column in the `target_path` file.

- target_start_col:

  Name of the genomic start position column in the `target_path` file.

- target_end_col:

  Name of the genomic end position column in the `target_path` file.

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

- convert_methods:

  A named list containing methods to run each step with.

- query_method:

  Method used for querying. See
  [query](https://rajlabmssm.github.io/echotabix/reference/query.md) for
  available options.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- convert_force_new:

  If the `target_path` is already in sorted/indexed tabix format, set
  `convert_force_new=TRUE` to re-convert it into tabix format.

- query_force_new:

  If the query subset (`query_save_path`) already exists, set
  `query_force_new=TRUE` to retrieve a new query subset.

- nThread:

  Number of threads to use.

- verbose:

  Print messages.

## Value

[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) or
[VCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html) of
requested subset of `target_path`.

## See also

Other tabix:
[`index_variantannotation()`](https://rajlabmssm.github.io/echotabix/reference/index_variantannotation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
query_dat <- echodata::BST1
target_path <- echodata::example_fullSS()

query_res <- echotabix::convert_and_query(
    target_path = target_path,
    target_start_col = "BP",
    query_granges = query_dat,
    query_force_new = TRUE)
} # }
```
