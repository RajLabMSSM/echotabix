# Construct the path to vcf subset

` locus_dir <- echodata::locus_dir BST1 <- echodata::BST1 vcf_subset <- construct_subset_vcf_name( dat = BST1, locus_dir = locus_dir, vcf_name = "1KGlocal" ) `

## Usage

``` r
construct_subset_vcf_name(dat, vcf_name = NULL, locus_dir, whole_vcf = FALSE)
```

## See also

Other LD:
[`get_locus_vcf_folder()`](https://rajlabmssm.github.io/echotabix/reference/get_locus_vcf_folder.md)
