# Run gunzip

Decompress a file using `gunzip`.

## Usage

``` r
run_gunzip(
  path,
  conda_env = "echoR_mini",
  method = c("R.utils", "conda"),
  outputs = c("command", "path", "data"),
  verbose = TRUE
)
```

## Arguments

- path:

  Path to file.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- method:

  Method to gunzip `path` with:

  "conda":

  :   Finds gunzip binary in the specified `conda_env` and uses it.

  "R.utils":

  :   Uses the
      [gunzip](https://henrikbengtsson.github.io/R.utils/reference/compressFile.html)
      function.

- outputs:

  Which outputs to return in a list. Can be one or more of the
  following:

  "command" :

  :   The command run to gunzip the file.

  "path" :

  :   Path to the gunzipped file.

  "data" :

  :   The data imported from the gunzipped file.

  If only one valid option is selected, the item will be returned
  without being embedded within a list.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
tmp <- tempfile(fileext = ".csv.gz")
data.table::fwrite(dat, tmp)
out <- echotabix::run_gunzip(path=tmp)
} # }
```
