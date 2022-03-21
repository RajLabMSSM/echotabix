test_that("multiplication works", {
  
    
    gr1 <- echotabix::construct_query(query_dat = echodata::BST1)
    gr2 <- echotabix::construct_query(query_dat = echodata::LRRK2)
    gr <- suppressWarnings(c(gr1, gr2))

    string <- echotabix::granges_to_string(gr=gr)
    testthat::expect_equal(string,"4:14737349-16737284,12:40582993-41614423")
})
