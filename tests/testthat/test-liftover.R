test_that("liftover works", {
    
    dat <- echodata::BST1

    #### hg19 ==> hg38 ####
    dat_lifted <- liftover(
        dat = dat,
        query_genome = "hg19",
        target_genome = "hg38"
    )
    testthat::expect_equal(nrow(dat_lifted), nrow(dat))
    poportion_positions_changes <- sum(dat_lifted$POS!=dat$POS)/nrow(dat)
    testthat::expect_equal(poportion_positions_changes,1)

    #### hg38 ==> hg19 ####
    dat_lifted2 <- liftover(
        dat = dat_lifted,
        query_genome = "hg38",
        target_genome = "hg19"
    )
    ## Make sure col order is the same
    cols <- colnames(dat) 
    testthat::expect_equal( dat_lifted2[,..cols], dat)
    testthat::expect_equal(nrow(dat_lifted2), nrow(dat_lifted))
    poportion_positions_changes <- sum(dat_lifted2$POS!=dat_lifted$POS)/
        nrow(dat_lifted2)
    testthat::expect_equal(poportion_positions_changes,1)

    #### hg19 ==> hg19 ####
    dat_lifted3 <- liftover(
        dat = dat, 
        query_genome = "hg19",
        target_genome = "hg19"
    )
    testthat::expect_equal(dat_lifted3, dat)
})
