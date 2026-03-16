# Construct tabix path

Given some summary stats file, construct a name for the resulting
sorted, bgzip-compressed tabix file.

## Usage

``` r
construct_tabix_path(target_path, study_dir = NULL)
```

## Arguments

- target_path:

  Path to full GWAS/QTL summary statistics file.

- study_dir:

  \[optional\] Path to study-specific subfolder.

## See also

Other tabix functions:
[`construct_vcf_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_vcf_path.md),
[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md),
[`index`](https://rajlabmssm.github.io/echotabix/reference/index.md),
[`query_table()`](https://rajlabmssm.github.io/echotabix/reference/query_table.md),
[`query_vcf()`](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md),
[`read_bgz()`](https://rajlabmssm.github.io/echotabix/reference/read_bgz.md),
[`run_bgzip()`](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)

## Examples

``` r
bgz_file <- echotabix::construct_tabix_path(
    target_path = "mysumstatsfile.vcf.tsv.gz")
```
