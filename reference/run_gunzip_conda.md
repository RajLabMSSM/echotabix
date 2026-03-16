# Run gunzip: conda

Support function for
[run_gunzip](https://rajlabmssm.github.io/echotabix/reference/run_gunzip.md).

## Usage

``` r
run_gunzip_conda(
  path,
  gunzip_ex = NULL,
  outputs = c("command", "path", "data"),
  save_path = gsub(".gz|.bgz", "", path),
  conda_env = "echoR_mini",
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

- save_path:

  File to save the results to.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- verbose:

  Print messages.
