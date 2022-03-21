test_that("convert_and_query works", {
  
    query_dat <- echodata::BST1
    target_path <- echodata::example_fullSS()
    
    #### ---- table tabix ---- ####
    #### Both hg19 ####
    query_res <- echotabix::convert_and_query( 
        target_path = target_path,
        target_start_col = "BP",
        query_start_pos = min(query_dat$POS),
        query_end_pos = min(query_dat$POS)+100000,
        query_chrom = query_dat$CHR[1],
        query_force_new = TRUE)
    testthat::expect_true(methods::is(query_res,"data.table"))
    testthat::expect_equal(nrow(query_res), 400)
    
    #### query_genome hg38 ####
    query_res2 <- echotabix::convert_and_query( 
        target_path = target_path,
        query_genome = "hg38", 
        target_start_col = "BP",
        query_start_pos = min(query_dat$POS),
        query_end_pos = min(query_dat$POS)+100000,
        query_chrom = query_dat$CHR[1],
        query_force_new = TRUE)
    testthat::expect_true(methods::is(query_res2,"data.table"))
    testthat::expect_equal(nrow(query_res2), 400)
    
    
    #### target_genome hg38 ####
    query_res3 <- echotabix::convert_and_query( 
        target_path = target_path,
        target_genome = "hg38", 
        target_start_col = "BP",
        query_start_pos = min(query_dat$POS),
        query_end_pos = min(query_dat$POS)+100000,
        query_chrom = query_dat$CHR[1],
        query_force_new = TRUE)
    testthat::expect_true(methods::is(query_res3,"data.table"))
    testthat::expect_equal(nrow(query_res2), 400)
    
    #### target_genome hg38 ####
    query_res3 <- echotabix::convert_and_query( 
        target_path = target_path,
        target_genome = "hg38", 
        target_start_col = "BP",
        query_start_pos = min(query_dat$POS),
        query_end_pos = min(query_dat$POS)+100000,
        query_chrom = query_dat$CHR[1],
        query_force_new = TRUE)
    testthat::expect_true(methods::is(query_res3,"data.table"))
    testthat::expect_equal(nrow(query_res2), 400)
    
    
})
