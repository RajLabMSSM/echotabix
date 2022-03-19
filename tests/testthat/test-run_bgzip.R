test_that("run_bgzip works", {
  
    dat <- echodata::BST1
    tmp <- tempfile(fileext = ".tsv.gz")
    data.table::fwrite(dat, tmp, sep="\t") 
    ### Sort 
    dat_sorted <- data.table::copy(dat)
    data.table::setkey(dat_sorted, CHR, POS)
    data.table::setkey(dat_sorted, NULL)
    
    #### Test missing args ####
    testthat::expect_error(
        bgz_file_err <- echotabix::run_bgzip(fullSS_path=tmp)
    )
    testthat::expect_error(
        bgz_file_err <- echotabix::run_bgzip(fullSS_path=tmp, 
                                         chrom_col = "CHR")
    )
    #### Test run: with .tsv: unsorted ####
    bgz_file2 <- echotabix::run_bgzip(fullSS_path=tmp, 
                                     chrom_col = "CHR", 
                                     start_col = "POS", 
                                     sort_rows = FALSE)
    dat1 <- echodata::read_bgz(bgz_file2)
    testthat::expect_equal(dat_sorted, dat1)
    
    #### Test run: with .tsv ####
    bgz_file1 <- echotabix::run_bgzip(fullSS_path=tmp, 
                                      chrom_col = "CHR", 
                                      start_col = "POS", 
                                      sort_rows = TRUE)
    dat1 <- echodata::read_bgz(bgz_file1)
    testthat::expect_equal(dat_sorted, dat1)
    
    
    #### Test run: with .csv ####
    tmp2 <- tempfile(fileext = ".csv.gz")
    data.table::fwrite(dat, tmp2, sep=",")
    bgz_file2 <- echotabix::run_bgzip(fullSS_path=tmp2, 
                                      chrom_col = "CHR", 
                                      start_col = "POS",
                                      method = "Rsamtools",
                                      sort_rows = TRUE)
    dat2 <- echodata::read_bgz(bgz_file2)
    testthat::expect_equal(dat_sorted, dat2)
})
