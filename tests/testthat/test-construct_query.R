test_that("construct_query works", {
  
    gr1 <- echotabix::construct_query(query_dat = echodata::BST1)
    gr2 <- echotabix::construct_query(query_dat = echodata::LRRK2)
    gr <- suppressWarnings(c(gr1, gr2)) 
    
    gr0 <- echotabix::construct_query(query_dat = gr)
    testthat::expect_equal(gr0, gr)
    
    #### Test related function: granges_to_string ####
    string <- echotabix::granges_to_string(gr=gr)
    testthat::expect_equal(string,"4:14737349-16737284,12:40582993-41614423")
})
