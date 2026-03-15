# Extracted from test-liftover.R:11

# test -------------------------------------------------------------------------
testthat::skip_on_ci()
dat <- echodata::BST1
dat_lifted <- echotabix::liftover(
        dat = dat,
        query_genome = "hg19",
        target_genome = "hg38"
    )
