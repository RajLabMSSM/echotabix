# Sort coordinates

Sort a table of summary statistics by their genomic coordinates without
reading the file into R.

## Usage

``` r
sort_coordinates(
  target_path,
  chrom_col,
  start_col,
  end_col = start_col,
  comment_char = NULL,
  save_path = NULL,
  method = c("bash", "data.table"),
  outputs = c("command", "path", "data"),
  verbose = TRUE
)
```

## Source

[sorting
instructions](https://www.systutorials.com/docs/linux/man/1-tabix/)

[StackExchange discussion of how `sort` handles
numbers](https://unix.stackexchange.com/a/382801)

## Arguments

- target_path:

  Path to full GWAS/QTL summary statistics file.

- chrom_col:

  Name of the chromosome column in the `target_path` file.

- start_col:

  Name of the genomic start position column in the `target_path` file.

- end_col:

  Name of the genomic end position column in the `target_path` file.

- comment_char:

  A single character which, when present as the first character in a
  line, indicates that the line is to be omitted from indexing.

- save_path:

  File to save the results to.

- method:

  Method to sort coordinates with.

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

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
tmp <- tempfile()
data.table::fwrite(dat, tmp)
out <- echotabix::sort_coordinates(target_path=tmp,
                                   chrom_col = "CHR",
                                   start_col = "POS")
} # }
```
