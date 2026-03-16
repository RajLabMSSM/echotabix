# Select method

Select a valid alternative method when the one chosen is not compatible
with the current Bioconductor version. When provided a valid method,
simply reutrns that method.

## Usage

``` r
select_method(
  fn,
  fn_arg = "method",
  method,
  rhtslib_pkgs = c("variantannotation", "rsamtools", "rtracklayer"),
  verbose = TRUE
)
```

## Arguments

- fn:

  Unevaluated function to select method for.

- fn_arg:

  Name of the argument that lists the various methods available for that
  particular function.

- method:

  Method selected by user.

- rhtslib_pkgs:

  List of R packages that depend on `Rhtslib`.

- verbose:

  Print messages.

## Value

A valid method.
