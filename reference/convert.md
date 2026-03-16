# Convert to tabix

Convert a tabular file to compressed (bgzip), indexed, tabix format for
rapid querying.

## Usage

``` r
convert(
  target_path,
  bgz_file = construct_tabix_path(target_path = target_path),
  chrom_col = "CHR",
  start_col = "POS",
  end_col = start_col,
  comment_char = NULL,
  format = NULL,
  convert_methods = list(sort_coordinates = if (.Platform$OS.type == "windows")
    "data.table" else "bash", run_bgzip = "Rsamtools", index = "Rsamtools"),
  conda_env = "echoR_mini",
  force_new = TRUE,
  verbose = TRUE
)
```

## Source

[samtools/tabix GitHub repo](https://github.com/samtools/htslib/issues)

[Rhtslib (which Rsamtools and seqminer depend on for tabix) is very out
of date (1.7 vs.
1.15)](https://github.com/Bioconductor/Rhtslib/issues/4).

[`conda install tabix` was recently fixed so it installs
htslib](https://github.com/bioconda/bioconda-recipes/issues/23404)

## Arguments

- target_path:

  Path to full GWAS/QTL summary statistics file.

- bgz_file:

  Path to resulting bgz-compressed file after `target_path` has been
  converted to tabix format.

- chrom_col:

  Name of the chromosome column in the `target_path` file.

- start_col:

  Name of the genomic start position column in the `target_path` file.

- end_col:

  Name of the genomic end position column in the `target_path` file.

- comment_char:

  A single character which, when present as the first character in a
  line, indicates that the line is to be omitted from indexing.

- format:

  Format of the `target_path` file: "vcf" or "table".

- convert_methods:

  A named list containing methods to run each step with.

- conda_env:

  Conda environments to search in. If `NULL` (default), will search all
  conda environments.

- force_new:

  Force the creation of a new bgzip file (*.bgz*) and a new tabix index
  file (*.tbi*).

- verbose:

  Print messages.

## See also

Other tabix functions:
[`construct_tabix_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_tabix_path.md),
[`construct_vcf_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_vcf_path.md),
[`index`](https://rajlabmssm.github.io/echotabix/reference/index.md),
[`query_table()`](https://rajlabmssm.github.io/echotabix/reference/query_table.md),
[`query_vcf()`](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md),
[`read_bgz()`](https://rajlabmssm.github.io/echotabix/reference/read_bgz.md),
[`run_bgzip()`](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)

## Examples

``` r
if (FALSE) { # \dontrun{
#### Example with full data ####
# tmp <- echodata::example_fullSS()
#### Example with single locus ####
dat <- echodata::BST1
tmp <- tempfile()
data.table::fwrite(dat, tmp, sep="\t")

tabix_files <- echotabix::convert(target_path = tmp)
} # }
```
