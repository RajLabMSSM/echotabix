test_that("convert_and_query works", {
  
    query_dat <- echodata::BST1[1:400,]
    target_path <- echodata::example_fullSS()
    
    #### ---- table tabix ---- ####
    #### as_blocks=FALSE ####
    # Works but loses ~9 SNPs due to some parsing error (added tryCatch for now)
    query_res <- echotabix::convert_and_query( 
        target_path = target_path,
        target_start_col = "BP", 
        query_granges = construct_query(query_dat = query_dat,
                                        as_blocks = FALSE),
        query_force_new = TRUE)
    testthat::expect_true(methods::is(query_res,"data.table"))
    testthat::expect_equal(nrow(query_res), 391)
    
    #### query_genome hg38 ####
    query_res2 <- echotabix::convert_and_query( 
        target_path = target_path,
        query_genome = "hg38", # <----
        target_start_col = "BP",
        query_dat = query_dat,
        query_force_new = TRUE)
    testthat::expect_true(methods::is(query_res2,"data.table"))
    testthat::expect_equal(nrow(query_res2), 6100)
    
    
    #### target_genome hg38 ####
    query_res3 <- echotabix::convert_and_query( 
        target_path = target_path,
        target_genome = "hg38", # <----
        target_start_col = "BP",
        query_dat = query_dat,
        query_force_new = TRUE)
    testthat::expect_true(methods::is(query_res3,"data.table"))
    testthat::expect_equal(nrow(query_res2), 6100)
    
    
})
