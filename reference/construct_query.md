# Construct query

Convert a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
containing variant-level summary statistics into a
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
object to be used for querying tabix-indexed files. Depending on the
parameters supplied, the resulting
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
object can contain one or more ranges, across one or more chromosomes.

## Usage

``` r
construct_query(
  query_chrom = NULL,
  query_start_pos = NULL,
  query_end_pos = query_start_pos,
  query_dat = NULL,
  query_chrom_col = "CHR",
  query_start_col = "POS",
  query_end_col = query_start_col,
  query_snp_col = "SNP",
  standardise_colnames = FALSE,
  as_blocks = TRUE,
  style = c("NCBI", "UCSC"),
  samples = character(),
  as_string = FALSE,
  verbose = TRUE
)
```

## Arguments

- query_chrom:

  Which chromosome to query (e.g. 1 or "chr1").

- query_start_pos:

  The first position to query.

- query_end_pos:

  The last position to query.

- query_dat:

  Variant-level summary statistics.

- query_chrom_col:

  Name of the chromosome column in `query_dat` (e.g. "CHR").

- query_start_col:

  Name of the starting genomic position column in `query_dat` (e.g.
  "POS","start").

- query_end_col:

  Name of the ending genomic position column in `query_dat` (e.g.
  "POS","end"). Can be the same as `query_start_col` when `query_dat`
  only contains SNPs that span 1 base pair (bp) each.

- query_snp_col:

  Name of the RSID or variant ID column (e.g. "SNP").

- standardise_colnames:

  Automatically rename all columns to a standard nomenclature using
  [standardise_header](https://al-murphy.github.io/MungeSumstats/reference/standardise_header.html).

- as_blocks:

  If `TRUE` (default), will query a single range per chromosome that
  covers all ranges requested plus anything in between.

- style:

  Style to return
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  in:

  "NCBI":

  :   Chromosome format: 1 (default)

  "UCSC":

  :   Chromosome format: "chr1"

  Uses
  [seqlevelsStyle](https://rdrr.io/pkg/GenomeInfoDb/man/seqlevelsStyle.html).

- samples:

  \[Optional\] Sample names to subset the VCF by. If this option is
  used, the
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object will be converted to a
  [ScanVcfParam](https://rdrr.io/pkg/VariantAnnotation/man/ScanVcfParam-class.html)
  for usage by
  [readVcf](https://rdrr.io/pkg/VariantAnnotation/man/readVcf-methods.html).

- as_string:

  Convert a
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object to a concatenated string of coordinates (e.g.
  "chr4:70000-90000,chr10:200-150001"). This can be used for specifying
  which regions you want to query (e.g. when using `tabix`). Uses
  [granges_to_string](https://rajlabmssm.github.io/echotabix/reference/granges_to_string.md).

- verbose:

  Print messages.

## Value

[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html) or
[ScanVcfParam](https://rdrr.io/pkg/VariantAnnotation/man/ScanVcfParam-class.html)
object.

## Parameters sets

If you supply the *Set 1* of parameters, you do not need to supply the
*Set 2* parameters (and vice versa). *Extra Parameters* apply to both
*Set 1* and *Set 2*.

- Set 1: :

  `query_chrom`, `query_start_pos`, `query_end_pos`

- Set 2: :

  `query_dat`, `query_chrom_col`, `query_start_col`, `query_end_col`,
  `as_blocks`

- Extra Parameters: :

  `style`, `samples`, `as_string`, `verbose`

## Examples

``` r
if (FALSE) { # \dontrun{
gr <- echotabix::construct_query(query_dat=echodata::BST1)
} # }
```
