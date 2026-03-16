# Query VCF: conda

Query a subset of a VCF file (remote or local) using `tabix` via
echoconda. **Advantages:**

- Fast.

- Not dependent on any R packages.

## Usage

``` r
query_vcf_conda(
  target_path,
  query_granges,
  samples = character(),
  download_index = FALSE,
  query_save = FALSE,
  save_path = NULL,
  conda_env = "echoR_mini",
  verbose = TRUE
)
```

## Arguments

- target_path:

  Path to local VCF file or remote URL.

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

- download_index:

  Whether to download the index when querying. Corresponds to the `-D`
  argument in tabix.

- query_save:

  Whether to save the results of the query on disk. *Note*: Writing to
  disk can take some time.

- save_path:

  File path to save query subset to (as VCF).

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- verbose:

  Print messages.

## Value

[CollapsedVCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html)
object.
