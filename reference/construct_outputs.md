# Construct outputs

Construct a list of requested outputs.

## Usage

``` r
construct_outputs(
  outputs,
  command = NULL,
  path = NULL,
  data = NULL,
  verbose = TRUE
)
```

## Arguments

- outputs:

  "command"

  :   Text string of the command (without executing it.)

  "path"

  :   Path to the saved data file.

  "data"

  :   The resulting data in
      [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
      format.

- command:

  Whether to return the command.

- path:

  Whether to return the path.

- data:

  Whether to return the data.

- verbose:

  Print messages.
