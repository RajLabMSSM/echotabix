# Query tabular: Rsamtools

Rsamtools can query both local and remote files. However, it seems
unable to read the header of tabix files created with seqminer.

## Usage

``` r
query_table_rsamtools(
  target_path,
  target_index = paste0(target_path, ".tbi"),
  query_granges,
  yieldSize = NA_character_,
  verbose = TRUE
)
```

## Source

[Bugs in
[`Rsamtools::scanTabix`](https://rdrr.io/pkg/Rsamtools/man/scanTabix.html)](https://github.com/Bioconductor/Rsamtools/issues/8)

## Arguments

- target_path:

  Path to tabix file.

- target_index:

  Tabix index file for `target_path`.

- query_granges:

  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object to be used for querying the `target_path` file. Alternatively,
  can be variant-level summary statistics to be converted into a
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object by
  [construct_query](https://rajlabmssm.github.io/echotabix/reference/construct_query.md).

- verbose:

  Print messages.
