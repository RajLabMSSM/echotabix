test_that("construct_tabix_path works", {
     
    bgz_file = echotabix::construct_tabix_path(
        target_path = "mysumstatsfile.tsv.gz"
    )
    testthat::expect_equal(bgz_file,"./mysumstatsfile.tsv.bgz")
})
