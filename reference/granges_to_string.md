# GRanges to string

Convert a
[GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
object to a concatenated string of coordinates (e.g.
"chr4:70000-90000,chr10:200-150001"). This can be used for specifying
which regions you want to query (e.g. when using `tabix`).

## Usage

``` r
granges_to_string(
  gr,
  pos_sep = "-",
  chrom_sep = ":",
  ranges_sep = ",",
  verbose = TRUE
)
```

## Arguments

- gr:

  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object.

- pos_sep:

  Character to separate start/end genomic positions with.

- chrom_sep:

  Character to separate chromosome name from start/end positions.

- ranges_sep:

  Character to separate each genomic range with.

- verbose:

  Print messages.

## Value

A concatenated string of coordinates.

## Examples

``` r
if (FALSE) { # \dontrun{
gr1 <- echotabix::construct_query(query_dat = echodata::BST1)
gr2 <- echotabix::construct_query(query_dat = echodata::LRRK2)
gr <- suppressWarnings(c(gr1, gr2))

string <- echotabix::granges_to_string(gr=gr)
} # }
```
