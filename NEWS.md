# echotabix 0.99.5

## New features

* Condense all query args further into `query_granges`.
* Add `standardise_colnames` arg to `construct_query`.
* Add `check_convert_methods` to validate `convert_methods` arg in `convert()`.

## Big fixes

* Passing all tests, just not when running *Test Package* in Rstudio. 
Weirdly, using the *Run Tests* button in each test script also works fine. 
Possibly related to [this Issue](https://github.com/r-lib/covr/issues/487)?
* Add `echoconda::yaml_to_env` call at the beginning of all 
conda-based functions.

# echotabix 0.99.4

## New features

* Add *Getting Started* vignette.
* Condense all query args into `query_dat` and `query_granges`.

# echotabix 0.99.3

## New features

* Added a `NEWS.md` file to track changes to the package.
* Updated GHA. 
* Removed *docs/* folder
