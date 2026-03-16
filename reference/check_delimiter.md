# Check delimiter

Automatically infer what delimiter is used to separate columns (e.g.
",", "\t"," "). Uses messages from
[fread](https://rdrr.io/pkg/data.table/man/fread.html) to extract this
inference.

## Usage

``` r
check_delimiter(path, verbose = TRUE)
```

## Arguments

- path:

  Path to file.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
path <- tempfile()
data.table::fwrite(dat, path, sep="\t")
delim <- echotabix::check_delimiter(path=path)
} # }
```
