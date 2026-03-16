# Read bgz

Read a compressed bgz file into R.

## Usage

``` r
read_bgz(
  path,
  method = c("data.table", "utils"),
  nrows = NULL,
  header = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- path:

  Path to bgz-compressed file.

- method:

  Method to use to import bgz file.

- nrows:

  The maximum number of rows to read. Unlike `read.table`, you do not
  need to set this to an estimate of the number of rows in the file for
  better speed because that is already automatically determined by
  `fread` almost instantly using the large sample of lines. `nrows=0`
  returns the column names and typed empty columns determined by the
  large sample; useful for a dry run of a large file or to quickly check
  format consistency of a set of files before starting to read any of
  them.

- header:

  Does the first data line contain column names? Defaults according to
  whether every non-empty field on the first data line is type
  character. If so, or TRUE is supplied, any empty column names are
  given a default name.

- verbose:

  Be chatty and report timings?

- ...:

  Additional arguments passed to
  [fread](https://rdrr.io/pkg/data.table/man/fread.html) or
  [read.delim2](https://rdrr.io/r/utils/read.table.html).

## Details

NOTE: Packages like Rsamtools and seqminer aren't great for this because
they require that the bgz file it also indexed already.

## See also

Other tabix functions:
[`construct_tabix_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_tabix_path.md),
[`construct_vcf_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_vcf_path.md),
[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md),
[`index`](https://rajlabmssm.github.io/echotabix/reference/index.md),
[`query_table()`](https://rajlabmssm.github.io/echotabix/reference/query_table.md),
[`query_vcf()`](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md),
[`run_bgzip()`](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tmp <- tempfile(fileext = ".tsv.gz")
data.table::fwrite(echodata::BST1, file = tmp, sep = "\t")
path <- Rsamtools::bgzip(file = tmp, overwrite=TRUE)
dat <- echotabix::read_bgz(path=path, method="utils")
} # }
```
