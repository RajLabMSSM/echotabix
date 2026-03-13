test_that("run_bgzip works", {

    dat <- echodata::BST1[1:100,]
    tmp <- tempfile(fileext = ".tsv.gz")
    data.table::fwrite(dat, tmp, sep="\t")
    ### Sort
    dat_sorted <- data.table::copy(dat)
    try({data.table::setkeyv(dat_sorted, c("CHR", "POS"))})

    #### Test missing args ####
    testthat::expect_error(
        echotabix::run_bgzip(target_path=tmp)
    )
    testthat::expect_error(
        echotabix::run_bgzip(target_path=tmp,
                             chrom_col = "CHR")
    )
    #### Test run: with .tsv: unsorted ####
    bgz_file2 <- echotabix::run_bgzip(target_path=tmp,
                                     chrom_col = "CHR",
                                     start_col = "POS",
                                     sort_rows = FALSE)
    dat1 <- echotabix::read_bgz(bgz_file2)
    ## CHR may be read back as character; coerce for comparison
    if(is.character(dat1$CHR)) dat1[, CHR := as.integer(CHR)]
    testthat::expect_equal(dat, dat1)

    #### Test run: with .tsv: sorted ####
    ## Recreate file since previous run consumed it
    tmp2 <- tempfile(fileext = ".tsv.gz")
    data.table::fwrite(dat, tmp2, sep="\t")
    bgz_file1 <- echotabix::run_bgzip(target_path=tmp2,
                                      chrom_col = "CHR",
                                      start_col = "POS",
                                      sort_rows = TRUE)
    dat2 <- echotabix::read_bgz(bgz_file1)
    if(is.character(dat2$CHR)) dat2[, CHR := as.integer(CHR)]
    testthat::expect_equal(dat_sorted, dat2)
})
