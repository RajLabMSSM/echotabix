# Sort coordinates: data.table

Support function for
[sort_coordinates](https://rajlabmssm.github.io/echotabix/reference/sort_coordinates.md).

## Usage

``` r
sort_coordinates_datatable(
  target_path,
  chrom_col,
  start_col,
  end_col = start_col,
  outputs = c("command", "path", "data"),
  skip = "__auto__",
  save_path = NULL,
  verbose = TRUE
)
```

## Arguments

- target_path:

  Path to full GWAS/QTL summary statistics file.

- chrom_col:

  Name of the chromosome column in the `target_path` file.

- start_col:

  Name of the genomic start position column in the `target_path` file.

- end_col:

  Name of the genomic end position column in the `target_path` file.

- outputs:

  "command"

  :   Text string of the command (without executing it.)

  "path"

  :   Path to the saved data file.

  "data"

  :   The resulting data in
      [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
      format.

- save_path:

  File to save the results to.

- verbose:

  Print messages.
