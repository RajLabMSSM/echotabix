# Getting Started

## Introduction

`echotabix` automates the creation and querying of
[tabix](http://www.htslib.org/doc/tabix.md)-indexed files. It handles
the full pipeline of liftover, sorting, compression, and indexing with a
single function call. Both local and remote tabix files (VCF or tabular)
can be queried, with automatic genome build detection and liftover when
needed.

## Installation

``` r

if (!require("remotes")) install.packages("remotes")
remotes::install_github("RajLabMSSM/echotabix")
```

``` r

library(echotabix)
```

## Convert

[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md)
takes a summary statistics file (or similar tabular genomic data) and
produces a sorted, bgzip-compressed, tabix-indexed file.

``` r

## Download example full summary statistics (requires internet)
tmp <- echodata::example_fullSS()
```

    ## Writing file to ==> /tmp/RtmpWVl84r/nalls2019.fullSS_subset.tsv

``` r

tabix_files <- echotabix::convert(target_path = tmp,
                                  start_col = "BP")
```

    ## ========= echotabix::convert =========

    ## Converting full summary stats file to tabix format for fast querying.

    ## Inferred format: 'table'

    ## Explicit format: 'table'

    ## Inferring comment_char from tabular header: 'SNP'

    ## Determining chrom type from file header.

    ## Chromosome format: 1

    ## Detecting column delimiter.

    ## Identified column separator: \t

    ## Sorting rows by coordinates via bash.

    ## Searching for header row with grep.

    ## ( grep ^'SNP' .../nalls2019.fullSS_subset.tsv; grep
    ##     -v ^'SNP' .../nalls2019.fullSS_subset.tsv | sort
    ##     -k2,2n
    ##     -k3,3n ) > .../file14c71075592_sorted.tsv

    ## Constructing outputs

    ## bgzipping file with Rsamtools.

    ## Reading bgzipped file using: data.table

    ## Header preview:

    ##           SNP   CHR       BP     A1     A2   FREQ    BETA     SE      P N_CAS
    ##        <char> <int>    <int> <char> <char>  <num>   <num>  <num>  <num> <int>
    ## 1:   rs741214     4 13737637      C      A 0.2341  0.0091 0.0119 0.4460 49053
    ## 2: rs79385021     4 13737659      T      C 0.9109  0.0009 0.0198 0.9642 30435
    ## 3:   rs741215     4 13737767      C      T 0.0733  0.0190 0.0194 0.3284 49053
    ## 4:   rs759261     4 13737915      G      A 0.4511  0.0075 0.0100 0.4543 49053
    ## 5: rs35006360     4 13738014      G      A 0.1148 -0.0195 0.0160 0.2208 49053
    ##      N_CON
    ##      <int>
    ## 1: 1411006
    ## 2:  974587
    ## 3: 1411006
    ## 4: 1411006
    ## 5: 1411006

    ## Tabix-indexing file using: Rsamtools

    ## Data successfully converted to bgzip-compressed, tabix-indexed format.

## Query

[`query()`](https://rajlabmssm.github.io/echotabix/reference/query.md)
automatically chooses the correct methods to perform a query by
inferring:

- Whether the file is local (e.g. on your computer) or remote (e.g. on a
  server accessed with a URL).
- Whether the file is in VCF or table format.
- Whether your query’s genome build (`query_genome`) matches the full
  data’s genome build (`target_genome`), and if not performs liftover
  before querying.

`query_granges` is used to specify which coordinates you want to query
from the target file. This argument is flexible and can take any of the
following:

1.  A `GRanges` object with one or more rows, across one or more
    chromosomes.
2.  A `data.table` containing the columns “CHR”, “POS”, and “SNP”, which
    is automatically converted to a `GRanges` object.
3.  The direct output of
    [`echotabix::construct_query()`](https://rajlabmssm.github.io/echotabix/reference/construct_query.md),
    for additional flexibility and non-standard column names.

### Local file

``` r

query_dat <- echodata::BST1

query_res <- echotabix::query(target_path = tabix_files$path,
                              query_granges = query_dat)
```

    ## ========= echotabix::query =========

    ## Constructing GRanges query using min/max ranges across one or more chromosomes.

    ## + as_blocks=TRUE: Will query a single range per chromosome that covers all regions requested (plus anything in between).

    ## Inferred format: 'table'

    ## Querying tabular tabix file using: Rsamtools.

    ## Checking query chromosome style is correct.

    ## Chromosome format: 1

    ## Creating TabixFile connection.

    ## Retrieving data.

    ## Converting query results to data.table.

    ## Processing query: 4:14737349-16737284

    ## Adding 'query' column to results.

    ## Retrieved data with 6,122 rows

    ## Saving query ==> /tmp/RtmpWVl84r/file14c768da53ea.gz

``` r

knitr::kable(head(query_res))
```

| query | SNP | CHR | BP | A1 | A2 | FREQ | BETA | SE | P | N_CAS | N_CON |
|:---|:---|---:|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| 4:14737349-16737284 | rs61337515 | 4 | 14737349 | C | T | 0.0215 | -0.0463 | 0.0403 | 0.2512 | 56306 | 1417791 |
| 4:14737349-16737284 | rs58950976 | 4 | 14738008 | G | A | 0.0177 | 0.0167 | 0.0422 | 0.6930 | 49053 | 1411006 |
| 4:14737349-16737284 | rs13152654 | 4 | 14738060 | G | C | 0.8623 | -0.0204 | 0.0145 | 0.1597 | 56306 | 1417791 |
| 4:14737349-16737284 | rs13103770 | 4 | 14738597 | C | G | 0.3712 | -0.0020 | 0.0099 | 0.8370 | 56306 | 1417791 |
| 4:14737349-16737284 | rs6837632 | 4 | 14738607 | A | C | 0.2284 | -0.0164 | 0.0116 | 0.1565 | 56306 | 1417791 |
| 4:14737349-16737284 | rs114563990 | 4 | 14738742 | A | G | 0.9576 | 0.0242 | 0.0247 | 0.3264 | 56306 | 1417791 |

### Customised queries

You can gain more customised control over the query with the helper
function
[`construct_query()`](https://rajlabmssm.github.io/echotabix/reference/construct_query.md)
(which is called internally by default when `query_granges` is not a
`GRanges` object).

#### Using min/max ranges

``` r

query_granges1 <- echotabix::construct_query(query_chrom = "chr4",
                                             query_start_pos = 5000,
                                             query_end_pos = 1e8L)
```

    ## Constructing GRanges query using min/max ranges within a single chromosome.

``` r

query_res2 <- echotabix::query(target_path = tabix_files$path,
                               query_granges = query_granges1)
```

    ## ========= echotabix::query =========

    ## query_dat is already a GRanges object. Returning directly.

    ## Inferred format: 'table'

    ## Querying tabular tabix file using: Rsamtools.

    ## Checking query chromosome style is correct.

    ## Chromosome format: 1

    ## Creating TabixFile connection.

    ## Retrieving data.

    ## Converting query results to data.table.

    ## Processing query: 4:5000-100000000

    ## Adding 'query' column to results.

    ## Retrieved data with 12,994 rows

    ## Saving query ==> /tmp/RtmpWVl84r/file14c759269660.gz

#### Using a query_dat object

``` r

query_dat2 <- echodata::LRRK2[1:100, ]
## Rename columns for demo purposes
data.table::setnames(query_dat2, c("CHR", "SNP"), c("seqnames", "rsID"))

query_granges2 <- echotabix::construct_query(query_dat = query_dat2,
                                             query_chrom_col = "seqnames",
                                             query_snp_col = "rsID")
```

    ## Constructing GRanges query using min/max ranges across one or more chromosomes.

    ## + as_blocks=TRUE: Will query a single range per chromosome that covers all regions requested (plus anything in between).

``` r

query_res3 <- echotabix::query(target_path = tabix_files$path,
                               query_granges = query_granges2)
```

    ## ========= echotabix::query =========

    ## query_dat is already a GRanges object. Returning directly.

    ## Inferred format: 'table'

    ## Querying tabular tabix file using: Rsamtools.

    ## Checking query chromosome style is correct.

    ## Chromosome format: 1

    ## Creating TabixFile connection.

    ## Retrieving data.

    ## Converting query results to data.table.

    ## Processing query: 12:40599398-41586834

    ## Adding 'query' column to results.

    ## Retrieved data with 3,626 rows

    ## Saving query ==> /tmp/RtmpWVl84r/file14c7787d8233.gz

#### Standardise column names

Instead of specifying each column name, you can also select
`standardise_colnames = TRUE` to automatically rename all columns to a
standard nomenclature using
[`MungeSumstats::format_header`](https://neurogenomics.github.io/MungeSumstats/reference/standardise_header.html).

``` r

query_granges3 <- echotabix::construct_query(query_dat = query_dat2,
                                             standardise_colnames = TRUE)
query_res4 <- echotabix::query(target_path = tabix_files$path,
                               query_granges = query_granges3)
```

### Remote file

Next, we query whole-genome sequencing data from the [1000 Genomes
Project](https://www.internationalgenome.org/). Specifically, we extract
a given genomic range for a subset of samples (individuals).

``` r

target_path <- file.path(
    "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/",
    "ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz"
)
## Fewer samples will speed up queries substantially (15-30s vs. >1.2min).
samples <- c("HG00097", "HG00099", "HG00100", "HG00101", "HG00102")
dat2 <- echodata::BST1[1:50, ]

#### Query ####
vcf <- echotabix::query(target_path = target_path,
                        query_granges = dat2,
                        samples = samples,
                        query_save = TRUE,
                        as_datatable = FALSE,
                        query_force_new = TRUE)
```

    ## ========= echotabix::query =========

    ## Constructing GRanges query using min/max ranges across one or more chromosomes.

    ## + as_blocks=TRUE: Will query a single range per chromosome that covers all regions requested (plus anything in between).

    ## Inferred format: 'vcf'

    ## Querying VCF tabix file.

    ## Querying VCF file using: VariantAnnotation

    ## Checking query chromosome style is correct.

    ## Chromosome format: 1

    ## Filtering query to 5 samples and returning ScanVcfParam object.

    ## Retrieving data.

    ## Time difference of 3.9 secs

    ## Saving VCF subset ==> /tmp/RtmpWVl84r/VCF/RtmpWVl84r.chr4-14884541-16649679.ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.bgz

    ## Time difference of 1.7 secs

    ## Retrieved data with 24,376 rows across 5 samples.

``` r

print(vcf)
```

    ## class: CollapsedVCF 
    ## dim: 24376 5 
    ## rowRanges(vcf):
    ##   GRanges with 5 metadata columns: paramRangeID, REF, ALT, QUAL, FILTER
    ## info(vcf):
    ##   DataFrame with 22 columns: LDAF, AVGPOST, RSQ, ERATE, THETA, CIEND, CIPOS,...
    ## info(header(vcf)):
    ##              Number Type    Description                                        
    ##    LDAF      1      Float   MLE Allele Frequency Accounting for LD             
    ##    AVGPOST   1      Float   Average posterior probability from MaCH/Thunder    
    ##    RSQ       1      Float   Genotype imputation quality from MaCH/Thunder      
    ##    ERATE     1      Float   Per-marker Mutation rate from MaCH/Thunder         
    ##    THETA     1      Float   Per-marker Transition rate from MaCH/Thunder       
    ##    CIEND     2      Integer Confidence interval around END for imprecise var...
    ##    CIPOS     2      Integer Confidence interval around POS for imprecise var...
    ##    END       1      Integer End position of the variant described in this re...
    ##    HOMLEN    .      Integer Length of base pair identical micro-homology at ...
    ##    HOMSEQ    .      String  Sequence of base pair identical micro-homology a...
    ##    SVLEN     1      Integer Difference in length between REF and ALT alleles   
    ##    SVTYPE    1      String  Type of structural variant                         
    ##    AC        .      Integer Alternate Allele Count                             
    ##    AN        1      Integer Total Allele Count                                 
    ##    AA        1      String  Ancestral Allele, ftp://ftp.1000genomes.ebi.ac.u...
    ##    AF        1      Float   Global Allele Frequency based on AC/AN             
    ##    AMR_AF    1      Float   Allele Frequency for samples from AMR based on A...
    ##    ASN_AF    1      Float   Allele Frequency for samples from ASN based on A...
    ##    AFR_AF    1      Float   Allele Frequency for samples from AFR based on A...
    ##    EUR_AF    1      Float   Allele Frequency for samples from EUR based on A...
    ##    VT        1      String  indicates what type of variant the line represents 
    ##    SNPSOURCE .      String  indicates if a snp was called when analysing the...
    ## geno(vcf):
    ##   List of length 3: GT, DS, GL
    ## geno(header(vcf)):
    ##       Number Type   Description                      
    ##    GT 1      String Genotype                         
    ##    DS 1      Float  Genotype dosage from MaCH/Thunder
    ##    GL .      Float  Genotype Likelihoods

## Convert and query

[`convert_and_query()`](https://rajlabmssm.github.io/echotabix/reference/convert_and_query.md)
merges the
[`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md)
and
[`query()`](https://rajlabmssm.github.io/echotabix/reference/query.md)
functions into a single pipeline.

- If the `target_path` file is not already a tabix file, it will first
  be converted to a sorted, bgzip-compressed, tabix-indexed file, and
  then proceed to the `query` step.
- If the `target_path` file is already a tabix file, the function will
  detect this automatically and skip right to the `query` step.

``` r

query_dat <- echodata::BST1
target_path2 <- echodata::example_fullSS()
```

    ## Writing file to ==> /tmp/RtmpWVl84r/nalls2019.fullSS_subset.tsv

``` r

query_res5 <- echotabix::convert_and_query(
    target_path = target_path2,
    target_start_col = "BP",
    query_granges = query_dat,
    query_force_new = TRUE)
```

    ## Constructing GRanges query using min/max ranges across one or more chromosomes.

    ## + as_blocks=TRUE: Will query a single range per chromosome that covers all regions requested (plus anything in between).

    ## Using existing tabix file: /tmp/RtmpWVl84r/nalls2019.fullSS_subset.tsv

    ## ========= echotabix::query =========

    ## query_dat is already a GRanges object. Returning directly.

    ## Inferred format: 'table'

    ## Querying tabular tabix file using: Rsamtools.

    ## Checking query chromosome style is correct.

    ## Chromosome format: 1

    ## Creating TabixFile connection.

    ## Retrieving data.

    ## Converting query results to data.table.

    ## Processing query: 4:14737349-16737284

    ## Adding 'query' column to results.

    ## Retrieved data with 6,122 rows

    ## Saving query ==> /tmp/RtmpWVl84r/file14c757c13792.gz

``` r

knitr::kable(head(query_res5))
```

| query | SNP | CHR | BP | A1 | A2 | FREQ | BETA | SE | P | N_CAS | N_CON |
|:---|:---|---:|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| 4:14737349-16737284 | rs61337515 | 4 | 14737349 | C | T | 0.0215 | -0.0463 | 0.0403 | 0.2512 | 56306 | 1417791 |
| 4:14737349-16737284 | rs58950976 | 4 | 14738008 | G | A | 0.0177 | 0.0167 | 0.0422 | 0.6930 | 49053 | 1411006 |
| 4:14737349-16737284 | rs13152654 | 4 | 14738060 | G | C | 0.8623 | -0.0204 | 0.0145 | 0.1597 | 56306 | 1417791 |
| 4:14737349-16737284 | rs13103770 | 4 | 14738597 | C | G | 0.3712 | -0.0020 | 0.0099 | 0.8370 | 56306 | 1417791 |
| 4:14737349-16737284 | rs6837632 | 4 | 14738607 | A | C | 0.2284 | -0.0164 | 0.0116 | 0.1565 | 56306 | 1417791 |
| 4:14737349-16737284 | rs114563990 | 4 | 14738742 | A | G | 0.9576 | 0.0242 | 0.0247 | 0.3264 | 56306 | 1417791 |

## Construct query (offline demo)

The
[`construct_query()`](https://rajlabmssm.github.io/echotabix/reference/construct_query.md)
function works entirely offline using bundled example datasets.

``` r

query_dat <- echodata::BST1
gr <- echotabix::construct_query(query_dat = query_dat)
```

    ## Constructing GRanges query using min/max ranges across one or more chromosomes.

    ## + as_blocks=TRUE: Will query a single range per chromosome that covers all regions requested (plus anything in between).

``` r

print(gr)
```

    ## GRanges object with 1 range and 1 metadata column:
    ##       seqnames            ranges strand |                    SNP
    ##          <Rle>         <IRanges>  <Rle> |            <character>
    ##   [1]        4 14737349-16737284      * | rs4541502;rs34559912..
    ##   -------
    ##   seqinfo: 1 sequence from an unspecified genome; no seqlengths

``` r

echotabix::granges_to_string(gr = gr)
```

    ## Converting GRanges query to a string of coordinates.

    ## [1] "4:14737349-16737284"

## Session Info

``` r

utils::sessionInfo()
```

    ## R Under development (unstable) (2026-03-12 r89607)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] echotabix_1.0.1  BiocStyle_2.39.0
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] DBI_1.3.0                   piggyback_0.1.5            
    ##   [3] bitops_1.0-9                rlang_1.1.7                
    ##   [5] magrittr_2.0.4              otel_0.2.0                 
    ##   [7] matrixStats_1.5.0           compiler_4.6.0             
    ##   [9] RSQLite_2.4.6               GenomicFeatures_1.63.1     
    ##  [11] dir.expiry_1.19.0           png_0.1-8                  
    ##  [13] systemfonts_1.3.2           vctrs_0.7.1                
    ##  [15] stringr_1.6.0               pkgconfig_2.0.3            
    ##  [17] crayon_1.5.3                fastmap_1.2.0              
    ##  [19] XVector_0.51.0              Rsamtools_2.27.1           
    ##  [21] rmarkdown_2.30              tzdb_0.5.0                 
    ##  [23] UCSC.utils_1.7.1            ragg_1.5.1                 
    ##  [25] purrr_1.2.1                 bit_4.6.0                  
    ##  [27] xfun_0.56                   cachem_1.1.0               
    ##  [29] cigarillo_1.1.0             GenomeInfoDb_1.47.2        
    ##  [31] jsonlite_2.0.0              blob_1.3.0                 
    ##  [33] DelayedArray_0.37.0         BiocParallel_1.45.0        
    ##  [35] echoconda_1.0.0             parallel_4.6.0             
    ##  [37] R6_2.6.1                    VariantAnnotation_1.57.1   
    ##  [39] bslib_0.10.0                stringi_1.8.7              
    ##  [41] reticulate_1.45.0           rtracklayer_1.71.3         
    ##  [43] GenomicRanges_1.63.1        jquerylib_0.1.4            
    ##  [45] Rcpp_1.1.1                  Seqinfo_1.1.0              
    ##  [47] bookdown_0.46               SummarizedExperiment_1.41.1
    ##  [49] knitr_1.51                  R.utils_2.13.0             
    ##  [51] readr_2.2.0                 IRanges_2.45.0             
    ##  [53] Matrix_1.7-4                tidyselect_1.2.1           
    ##  [55] abind_1.4-8                 yaml_2.3.12                
    ##  [57] codetools_0.2-20            curl_7.0.0                 
    ##  [59] lattice_0.22-9              tibble_3.3.1               
    ##  [61] Biobase_2.71.0              basilisk.utils_1.23.1      
    ##  [63] KEGGREST_1.51.1             evaluate_1.0.5             
    ##  [65] desc_1.4.3                  zip_2.3.3                  
    ##  [67] Biostrings_2.79.5           pillar_1.11.1              
    ##  [69] BiocManager_1.30.27         filelock_1.0.3             
    ##  [71] MatrixGenerics_1.23.0       DT_0.34.0                  
    ##  [73] stats4_4.6.0                generics_0.1.4             
    ##  [75] RCurl_1.98-1.17             S4Vectors_0.49.0           
    ##  [77] hms_1.1.4                   glue_1.8.0                 
    ##  [79] tools_4.6.0                 BiocIO_1.21.0              
    ##  [81] data.table_1.18.2.1         openxlsx_4.2.8.1           
    ##  [83] BSgenome_1.79.1             GenomicAlignments_1.47.0   
    ##  [85] fs_1.6.7                    XML_3.99-0.22              
    ##  [87] grid_4.6.0                  tidyr_1.3.2                
    ##  [89] echodata_1.0.0              AnnotationDbi_1.73.0       
    ##  [91] basilisk_1.23.0             restfulr_0.0.16            
    ##  [93] cli_3.6.5                   textshaping_1.0.5          
    ##  [95] S4Arrays_1.11.1             dplyr_1.2.0                
    ##  [97] R.methodsS3_1.8.2           sass_0.4.10                
    ##  [99] digest_0.6.39               BiocGenerics_0.57.0        
    ## [101] SparseArray_1.11.11         rjson_0.2.23               
    ## [103] htmlwidgets_1.6.4           memoise_2.0.1              
    ## [105] htmltools_0.5.9             pkgdown_2.2.0              
    ## [107] R.oo_1.27.1                 lifecycle_1.0.5            
    ## [109] httr_1.4.8                  bit64_4.6.0-1
