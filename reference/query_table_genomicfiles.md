# Query tabular: GenomicFiles

GenomicFiles can query both local and remote files.

## Usage

``` r
query_table_genomicfiles(
  target_path,
  target_index = paste0(target_path, ".tbi"),
  query_granges,
  yieldSize = NA_character_,
  verbose = TRUE
)
```

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
