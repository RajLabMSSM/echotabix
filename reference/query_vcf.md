# Query VCF file

Query a Variant Call Format (VCF) file. The VCF file can be either local
or remote.

## Usage

``` r
query_vcf(
  target_path,
  target_index = paste0(target_path, ".tbi"),
  target_genome = "GRCh37",
  query_granges,
  samples = character(),
  method = c("variantannotation", "conda", "rtracklayer", "seqminer"),
  overlapping_only = FALSE,
  query_save = TRUE,
  save_path = construct_vcf_path(target_path = target_path, query_granges =
    query_granges),
  force_new = FALSE,
  as_datatable = FALSE,
  cleanup_tbi = TRUE,
  conda_env = "echoR_mini",
  verbose = TRUE
)
```

## Arguments

- target_path:

  Path to local VCF file or remote URL.

- target_index:

  Tabix index file for `target_path`.

- target_genome:

  Genome build of the VCF file.

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

- method:

  Method to query VCF with.

- overlapping_only:

  Remove variants that do not overlap with the positions in `query_dat`.

- query_save:

  Whether to save the results of the query on disk. *Note*: Writing to
  disk can take some time.

- save_path:

  File path to save query subset to (as VCF).

- force_new:

  Force the creation of a new VCF subset file even if one exists.

- as_datatable:

  Return the VCF subset file as a
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  (using
  [vcf_to_dt](https://rajlabmssm.github.io/echotabix/reference/vcf_to_dt.md)).
  If `save_path=TRUE` the file will still be saved as a bgzip-compressed
  VCF file.

- cleanup_tbi:

  Remove local copies of tabix index file (*.tbi*) after completing
  queries.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- verbose:

  Print messages.

## Value

[VCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html) object,
or [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
(when `as_datatable=TRUE`).

## See also

Other tabix functions:
[`construct_tabix_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_tabix_path.md),
[`construct_vcf_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_vcf_path.md),
[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md),
[`index`](https://rajlabmssm.github.io/echotabix/reference/index.md),
[`query_table()`](https://rajlabmssm.github.io/echotabix/reference/query_table.md),
[`read_bgz()`](https://rajlabmssm.github.io/echotabix/reference/read_bgz.md),
[`run_bgzip()`](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)

## Examples

``` r
if (FALSE) { # \dontrun{
query_dat <- echodata::BST1
target_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                    package = "echodata")

#### Import ####
vcf <-  query_vcf(
    query_granges = query_dat,
    target_path = target_path)
} # }
```
