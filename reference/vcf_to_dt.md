# Variant Call Format (VCF) –\> data.table

Function to convert a
[VCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html) object
to a [data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

## Usage

``` r
vcf_to_dt(
  vcf,
  add_sample_names = TRUE,
  add_rowranges = TRUE,
  standardise_colnames = TRUE,
  verbose = TRUE
)
```

## Arguments

- vcf:

  Variant Call Format (VCF) file imported into R as a VariantAnnotation
  [CollapsedVCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html)/
  [ExpandedVCF](https://rdrr.io/pkg/VariantAnnotation/man/VCF-class.html)
  object.

- add_sample_names:

  Append sample names to column names (e.g. "EZ" –\> "EZ_ubm-a-2929").

- add_rowranges:

  Include `rowRanges` from VCF as well.

- standardise_colnames:

  Automatically rename all columns to a standard nomenclature using
  [standardise_header](https://al-murphy.github.io/MungeSumstats/reference/standardise_header.html).

- verbose:

  Print messages.

## Value

data.frame version of VCF

## Examples

``` r
if (FALSE) { # \dontrun{
vcf_file <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                        package = "echodata")
vcf <- VariantAnnotation::readVcf(file = vcf_file)
vcf_dt <- echotabix::vcf_to_dt(vcf = vcf)
} # }
```
