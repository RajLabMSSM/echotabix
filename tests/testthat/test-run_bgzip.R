test_that("run_bgzip works", {

    dat <- echodata::BST1[1:100,]
    ## Detect the position column name (POS or BP depending on echodata version)
    pos_col <- intersect(c("POS","BP"), colnames(dat))[1]
    if(is.na(pos_col)) testthat::skip("No position column (POS/BP) found in BST1")
    ## Use uncompressed .tsv to avoid gzip/bgzip double-compression issues
    tmp <- tempfile(fileext = ".tsv")
    data.table::fwrite(dat, tmp, sep="\t")
    ### Sort
    dat_sorted <- data.table::copy(dat)
    try({data.table::setkeyv(dat_sorted, c("CHR", pos_col))})

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
                                     start_col = pos_col,
                                     sort_rows = FALSE)
    dat1 <- echotabix::read_bgz(bgz_file2)
    ## Coerce types for comparison
    if(is.character(dat1$CHR)) dat1[, CHR := as.integer(CHR)]
    if(is.character(dat$CHR)) dat[, CHR := as.integer(CHR)]
    testthat::expect_equal(dat, dat1, check.attributes = FALSE)

    #### Test run: with .tsv: sorted ####
    ## Recreate file since previous run consumed it
    tmp2 <- tempfile(fileext = ".tsv")
    data.table::fwrite(dat, tmp2, sep="\t")
    bgz_file1 <- echotabix::run_bgzip(target_path=tmp2,
                                      chrom_col = "CHR",
                                      start_col = pos_col,
                                      sort_rows = TRUE)
    dat2 <- echotabix::read_bgz(bgz_file1)
    if(is.character(dat2$CHR)) dat2[, CHR := as.integer(CHR)]
    if(is.character(dat_sorted$CHR)) dat_sorted[, CHR := as.integer(CHR)]
    testthat::expect_equal(dat_sorted, dat2, ignore_attr = TRUE)
})
