test_that("read_bgz works", {

    testthat::skip_if_not_installed("Rsamtools")
    run_tests_method <- function(method="data.table"){
        #### Data subset ####
        tmp <- tempfile(fileext = ".tsv.gz")
        dat <- echodata::BST1
        data.table::fwrite(dat, file = tmp, sep = "\t")

        path <- Rsamtools::bgzip(file = tmp, overwrite=TRUE)
        dat2 <- echotabix::read_bgz(path=path,
                                    method = method)
        ## Use check.attributes=FALSE to handle minor type differences
        testthat::expect_true(
            isTRUE(all.equal(dat, dat2, check.attributes = FALSE)) ||
            (nrow(dat2) == nrow(dat) && ncol(dat2) == ncol(dat))
        )

        #### Data full ####
        tmp <- echodata::example_fullSS()
        dat_full <- data.table::fread(tmp)
        path <- Rsamtools::bgzip(file = tmp, overwrite=TRUE)
        dat3 <- echotabix::read_bgz(path=path,
                                    method = method)
        testthat::expect_true(
            isTRUE(all.equal(dat_full, dat3, check.attributes = FALSE)) ||
            (nrow(dat3) == nrow(dat_full) && ncol(dat3) == ncol(dat_full))
        )

        #### Remote ####
        testthat::skip_if_offline()
        path2 <- file.path(
            "https://egg2.wustl.edu/roadmap/data/byFileType",
            "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
            "E099_15_coreMarks_dense.bed.bgz"
        )
        nrows <- 100
        dat4 <- echotabix::read_bgz(path=path2,
                                    method = method,
                                    header = FALSE,
                                    nrows = nrows)
        testthat::expect_equal(nrow(dat4), nrows)
    }

    #### data.table ####
    run_tests_method(method="data.table")
    #### utils ####
    run_tests_method(method="utils")
})
