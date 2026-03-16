# Query tabular: conda

Uses a conda-based installation of tabix instead of compiled `C` code
from Rhtslib.

## Usage

``` r
query_table_conda(
  target_path,
  target_index,
  query_granges,
  force = FALSE,
  preset = NULL,
  skip_lines = NULL,
  print_header = TRUE,
  use_regions_file = TRUE,
  conda_env = "echoR_mini",
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

- force:

  Overwrite existing index without asking.

- preset:

  gff, bed, sam, vcf.

- skip_lines:

  skip first INT lines \[0\].

- print_header:

  Print also the header lines.

- use_regions_file:

  Specify query coordinates by writing them to a temporary file and
  supplying the file path to the "-R" argument.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- verbose:

  Print messages.
