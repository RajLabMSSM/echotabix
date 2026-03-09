test_that("query works", {

    testthat::skip_if_not_installed("Rsamtools")
    query_dat <- echodata::BST1

    #### Detect the position column in the full summary stats ####
    target_path <- echodata::example_fullSS()
    header <- colnames(data.table::fread(target_path, nrows = 0))
    start_col <- intersect(c("BP","POS","pos","bp"), header)[1]
    if(is.na(start_col)) start_col <- "POS"

    #### local ####
    tabix_files <- echotabix::convert(target_path = target_path,
                                      start_col = start_col)
    query_res <- echotabix::query(
        target_path = tabix_files$path,
        query_granges = query_dat)

    testthat::expect_gte(nrow(query_res), 5000)
    testthat::expect_lte(nrow(query_res), 7000)
})
