# scanTabix to data.table

Convert the output of
[scanTabix](https://rdrr.io/pkg/Rsamtools/man/scanTabix.html) into a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html). Can
handle tabix files that are missing column names, and query results that
only have a single row.

## Usage

``` r
scanTabix_to_dt(
  header,
  queries,
  add_query_names = TRUE,
  remove_duplicates = TRUE,
  sep = "\t",
  verbose = TRUE
)
```

## Arguments

- header:

  Header, from the output of
  [headerTabix](https://rdrr.io/pkg/Rsamtools/man/headerTabix.html).

- queries:

  A named list of query results, from the output of
  [scanTabix](https://rdrr.io/pkg/Rsamtools/man/scanTabix.html).

- add_query_names:

  Add the names of each query to a new column named 'query'.

- remove_duplicates:

  Remove any duplicated rows. Set `add_query_names=FALSE` to prevent
  each unique range (e.g. SNP) from appearing in more than one row.

- sep:

  The separator between columns. Defaults to the character in the set
  `[,\t |;:]` that separates the sample of rows into the most number of
  lines with the same number of fields. Use `NULL` or `""` to specify no
  separator; i.e. each line a single character column like
  [`base::readLines`](https://rdrr.io/r/base/readLines.html) does.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
fl <- system.file("extdata", "example.gtf.gz", package="Rsamtools",
                  mustWork=TRUE)
tbx <- Rsamtools::TabixFile(fl)

param <- GenomicRanges::GRanges(
    c("chr1", "chr2"),
    IRanges::IRanges(c(1, 1), width=100000))
queries <- Rsamtools::scanTabix(tbx, param=param)
header <- Rsamtools::headerTabix(fl)

#### Convert ####
query_dt <-  echotabix::scanTabix_to_dt(header = header,
                                        queries = queries)
} # }
```
