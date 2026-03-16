# Run gunzip: R.utils

Support function for
[run_gunzip](https://rajlabmssm.github.io/echotabix/reference/run_gunzip.md).

## Usage

``` r
run_gunzip_rutils(
  path,
  outputs = c("command", "path", "data"),
  overwrite = TRUE,
  remove = FALSE,
  verbose = TRUE
)
```

## Arguments

- path:

  Path to file.

- outputs:

  "command"

  :   Text string of the command (without executing it.)

  "path"

  :   Path to the saved data file.

  "data"

  :   The resulting data in
      [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
      format.

- verbose:

  Print messages.
