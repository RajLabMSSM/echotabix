test_that("liftover works", {
    BST1 <- echodata::BST1

    #### hg19 ==> hg38 ####
    dat_lifted <- liftover(
        sumstats_dt = BST1,
        ref_genome = "hg19",
        convert_ref_genome = "hg38"
    )
    testthat::expect_equal(nrow(dat_lifted), nrow(BST1))

    #### hg38 ==> hg19 ####
    dat_lifted2 <- liftover(
        sumstats_dt = dat_lifted,
        start_col = "BP",
        ref_genome = "hg38",
        convert_ref_genome = "hg19"
    )
    testthat::expect_equal(nrow(dat_lifted2), nrow(BST1))
    testthat::expect_equal(nrow(dat_lifted2), nrow(dat_lifted))

    #### hg19 ==> hg19 ####
    dat_lifted3 <- liftover(
        sumstats_dt = BST1,
        start_col = "POS",
        ref_genome = "hg19",
        convert_ref_genome = "hg19"
    )
    testthat::expect_equal(nrow(dat_lifted3), nrow(BST1))
})
