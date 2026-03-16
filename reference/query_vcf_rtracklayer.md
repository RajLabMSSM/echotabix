# Query VCF: rtracklayer

Query a subset of a VCF file (remote or local) using
`rtracklayer::import`, which is essentially just a wrapper for
[scanTabix](https://rdrr.io/pkg/Rsamtools/man/scanTabix.html).
**Advantages of rtracklayer:**

- None to speak of.

**Disadvantages of rtracklayer:**

- Unable to query a subset of samples, unlike
  [scanVcf](https://rdrr.io/pkg/VariantAnnotation/man/scanVcf-methods.html).

- Unable to return results as a structured
  [CollapsedVCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html)
  object.

## Usage

``` r
query_vcf_rtracklayer(
  target_path,
  target_index = paste0(target_path, ".tbi"),
  query_granges,
  samples = character(),
  query_save = FALSE,
  save_path = NULL,
  verbose = TRUE
)
```

## Source

[Inconsistencies with
rtracklayer](https://github.com/lawremi/rtracklayer/issues/62)

` BST1 <- echodata::BST1 query_dat <- BST1[seq(1, 50), ] target_path <- paste( "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/", "ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz", sep="/" ) vcf <- echotabix:::query_vcf_rtracklayer( target_path = target_path, query_granges = query_dat) `

## Arguments

- target_path:

  Path to local VCF file or remote URL.

- target_index:

  Tabix index file for `target_path`.

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

  Whether to save the results of the query on disk. *Note*: Writing to
  disk can take some time.

- save_path:

  File path to save query subset to (as VCF).

- verbose:

  Print messages.

## Value

Raw text?
