# Genome build liftover

Transfer genomic coordinates from one genome build to another.

## Usage

``` r
liftover(
  dat,
  query_genome,
  target_genome,
  query_chrom_col = "CHR",
  query_start_col = "POS",
  query_end_col = query_start_col,
  pos_prefix = "POS",
  as_granges = FALSE,
  style = "NCBI",
  verbose = TRUE
)
```

## Source

[liftOver](https://doi.org/doi:10.18129/B9.bioc.liftOver)

[UCSC chain
files](https://hgdownload.cse.ucsc.edu/goldenpath/hg19/liftOver/)

## Arguments

- dat:

  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) of
  GWAS/QTL summary statistics.

- query_genome:

  name of the reference genome used for the GWAS (e.g. "GRCh37" or
  "GRCh38"). Argument is case-insensitive.

- target_genome:

  name of the reference genome to convert to ("GRCh37" or "GRCh38").
  This will only occur if the `query_genome` does not match the
  `target_genome`.

- query_chrom_col:

  Name of the chromosome column in `query_dat` (e.g. "CHR").

- query_start_col:

  Name of the starting genomic position column in `query_dat` (e.g.
  "POS","start").

- query_end_col:

  Name of the ending genomic position column in `query_dat` (e.g.
  "POS","end"). Can be the same as `query_start_col` when `query_dat`
  only contains SNPs that span 1 base pair (bp) each.

- pos_prefix:

  After the `start_col``end_col` has been lifted over, how should these
  columns be named?

- as_granges:

  Return results as
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  instead of a
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  (default: `FALSE`).

- style:

  Style to return
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object in (e.g. "NCBI" = 4; "UCSC" = "chr4";) (default: `"NCBI"`).

- verbose:

  Print messages.

## Value

Lifted summary stats in `data.table` or
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
format.

## See also

Other liftover functions:
[`get_chain_file()`](https://rajlabmssm.github.io/echotabix/reference/get_chain_file.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
#### hg19 ==> hg38 ####
dat_lifted <- liftover(
    dat = dat,
    query_genome = "hg19",
    target_genome = "hg38"
)
} # }
```
