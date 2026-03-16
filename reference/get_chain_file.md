# Download chain file for liftover

Download chain file for liftover

## Usage

``` r
get_chain_file(
  build_conversion = c("hg38ToHg19", "hg19ToHg38"),
  save_dir = tools::R_user_dir(package = "echotabix", which = "cache"),
  timeout = 60 * 5,
  verbose = TRUE
)
```

## Source

[UCSC chain
files](https://hgdownload.cse.ucsc.edu/goldenpath/hg19/liftOver/)

## Arguments

- build_conversion:

  Converting from what build to what? "hg38ToHg19" or "hg19ToHg38"

- save_dir:

  Where is the chain file saved? Default is a temp directory

- verbose:

  extra messages printed? Default is TRUE

## Value

Loaded chain file for liftover.

## See also

Other liftover functions:
[`liftover()`](https://rajlabmssm.github.io/echotabix/reference/liftover.md)
