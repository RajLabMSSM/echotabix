# echotabix 0.99.6

* Moved all VCF examples to `example_data`.
* `convert` can now handle VCFs as well as table inputs
(though can only export as the same format). 
* `query_vcf`: Add alternative methods.
    - "variantannotation": ideal, but package is broken currently due 
    to issues with `Rsamtools`/`Rhtslib`.
    - "conda": Uses `echoconda` (*default* until "variantannotation" is fixed").
    - "seqminer": Generates table, but not fully parsed.
    - "rtracklayer": Generates table, but not fully parsed.

# echotabix 0.99.5

## New features

* Condense all query args further into `query_granges`.
* Added `standardise_colnames` arg to `construct_query`.
* Added `check_convert_methods` to validate `convert_methods` arg in `convert()`.

## Bug fixes

* Passing all tests, just not when running *Test Package* in Rstudio. 
Weirdly, using the *Run Tests* button in each test script also works fine. 
Possibly related to [this Issue](https://github.com/r-lib/covr/issues/487)?
* Add `echoconda::yaml_to_env` call at the beginning of all 
conda-based functions.
* Removed `subset_common_snps` function (already in `echoLD`).

# echotabix 0.99.4

## New features

* Add *Getting Started* vignette.
* Condense all query args into `query_dat` and `query_granges`.

# echotabix 0.99.3

## New features

* Added a `NEWS.md` file to track changes to the package.
* Updated GHA. 
* Removed *docs/* folder
