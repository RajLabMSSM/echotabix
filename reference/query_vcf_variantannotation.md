# Query VCF: VariantAnnotation

Query a subset of a VCF file (remote or local) using
[readVcf](https://rdrr.io/pkg/VariantAnnotation/man/readVcf-methods.html).
**Advantages of VariantAnnotation:**

- Is at least as fast as
  [scanTabix](https://rdrr.io/pkg/Rsamtools/man/scanTabix.html).

- Can query a specific subset of samples, unlike
  [scanTabix](https://rdrr.io/pkg/Rsamtools/man/scanTabix.html) which
  queries all samples at once.

- Automatically imports query results as a
  [CollapsedVCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html)
  object, which contain lots of organized information about the query
  data and can be further processed using other functions from
  VariantAnnotation and snpStats. By contrast,
  [scanTabix](https://rdrr.io/pkg/Rsamtools/man/scanTabix.html) returns
  a raw list of strings that must be parsed by the user.

## Usage

``` r
query_vcf_variantannotation(
  target_path,
  target_index = paste0(target_path, ".tbi"),
  target_genome = NULL,
  query_granges,
  samples = character(),
  verbose = TRUE
)
```

## Source

[VariantAnnotation filtering
vignette](https://bioconductor.org/packages/devel/bioc/vignettes/TVTB/inst/doc/VcfFilterRules.html)

[`gwasvcf` GitHub
repo](https://github.com/MRCIEU/gwasvcf/blob/master/R/query.r)

` BST1 <- echodata::BST1 query_dat <- BST1[seq(1, 50), ] target_path <- paste( "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/", "ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz", sep="/" ) vcf <- echotabix:::query_vcf_variantannotation( target_path = target_path, query_granges = query_dat) `

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

- verbose:

  Print messages.

## Value

[CollapsedVCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html)
object.
