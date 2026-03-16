# VCF path

Construct the `save_path` to VCF subset extracted by
[query_vcf](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md).

## Usage

``` r
construct_vcf_path(
  target_path,
  query_granges = NULL,
  locus_dir = tempdir(),
  subdir = "VCF",
  use_coord_prefix = TRUE,
  whole_vcf = FALSE
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

- locus_dir:

  Locus-specific folder.

- subdir:

  Subdirectory to store VCF in.

- use_coord_prefix:

  Add min/max genomic coordinates (e.g. "chr4-14737349-16737284") to the
  file name.

- whole_vcf:

  Whether to download the entire VCF (not just a subset).

## See also

Other tabix functions:
[`construct_tabix_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_tabix_path.md),
[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md),
[`index`](https://rajlabmssm.github.io/echotabix/reference/index.md),
[`query_table()`](https://rajlabmssm.github.io/echotabix/reference/query_table.md),
[`query_vcf()`](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md),
[`read_bgz()`](https://rajlabmssm.github.io/echotabix/reference/read_bgz.md),
[`run_bgzip()`](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)

## Examples

``` r
if (FALSE) { # \dontrun{
target_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                           package = "echodata")
locus_dir <- file.path(tempdir(), echodata::locus_dir)
query_granges <- echotabix::construct_query(query_dat=echodata::BST1)

save_path <- echotabix::construct_vcf_path(query_granges = query_granges,
                                           locus_dir = locus_dir,
                                           target_path = target_path)
} # }
```
