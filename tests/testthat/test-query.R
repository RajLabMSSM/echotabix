test_that("query works", {
  
    query_dat <- echodata::BST1

    #### local ####
    target_path <- echodata::example_fullSS()
    tabix_files <- echotabix::convert(target_path = target_path,
                                      start_col = "BP")
    query_res <- echotabix::query(
        target_path = tabix_files$path,
        query_granges = query_dat)
    
    testthat::expect_equal(nrow(query_res), 6122)
})
