test_that("tabix_path works", {
  
    bgz_file = echotabix::tabix_path(path = "mysumstatsfile.tsv.gz")
    testthat::expect_equal(bgz_file,"./mysumstatsfile.tsv.bgz")
})
