test_that("run_bgzip works", {
  
    dat <- echodata::BST1[1:100,]
    tmp <- tempfile(fileext = ".tsv.gz")
    data.table::fwrite(dat, tmp, sep="\t") 
    ### Sort 
    dat_sorted <- data.table::copy(dat)
    try({data.table::setkeyv(dat_sorted, c("CHR", "POS"))})
    # data.table::setkey(dat_sorted, NULL)
    
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
    testthat::expect_equal(dat, dat1)
    
    #### Test run: with .tsv ####
    bgz_file1 <- echotabix::run_bgzip(target_path=tmp, 
                                      chrom_col = "CHR", 
                                      start_col = "POS", 
                                      sort_rows = TRUE)
    dat1 <- echotabix::read_bgz(bgz_file1)
    testthat::expect_equal(dat_sorted, dat1) 
})
