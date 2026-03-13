test_that("liftover works", {

    testthat::skip_on_ci()
    dat <- echodata::BST1

    #### hg19 ==> hg38 ####
    dat_lifted <- liftover(
        dat = dat,
        query_genome = "hg19",
        target_genome = "hg38"
    )
    testthat::expect_equal(nrow(dat_lifted), nrow(dat))
    proportion_positions_changed <- sum(dat_lifted$POS!=dat$POS)/nrow(dat)
    testthat::expect_equal(proportion_positions_changed,1)

    #### hg38 ==> hg19 ####
    dat_lifted2 <- echotabix::liftover(
        dat = dat_lifted,
        query_genome = "hg38",
        target_genome = "hg19"
    )
    ## Make sure col order is the same
    cols <- colnames(dat)
    ## CHR may have chr prefix after liftover roundtrip
    dat_compare <- data.table::copy(dat_lifted2[,cols,with=FALSE])
    dat_compare[, CHR := as.integer(gsub("chr", "", CHR))]
    testthat::expect_equal(dat_compare, dat)
    testthat::expect_equal(nrow(dat_lifted2), nrow(dat_lifted))
    proportion_positions_changed <- sum(dat_lifted2$POS!=dat_lifted$POS)/
        nrow(dat_lifted2)
    testthat::expect_equal(proportion_positions_changed,1)

    #### hg19 ==> hg19 ####
    dat_lifted3 <- echotabix::liftover(
        dat = dat,
        query_genome = "hg19",
        target_genome = "hg19"
    )
    testthat::expect_equal(dat_lifted3, dat)
})
