test_that("sort_coordinates works", {
    
    tmp <- echodata::example_fullSS()
    dat <- data.table::fread(tmp)
    # dat <- echodata::BST1[1:200,]
    ## Sort alphanumerically by SNP instead of coordinates 
    data.table::setkey(dat, SNP)
    data.table::setkey(dat, NULL)
    # tmp <- tempfile()
    # data.table::fwrite(dat, tmp, sep="\t")
    tmp <- R.utils::gzip(tmp)
    ### sort ####
    dat_sorted <- data.table::copy(dat)
    data.table::setkey(dat_sorted, CHR, BP)
    ### Remove key 
    data.table::setkey(dat_sorted, NULL)
    #### Check that only a very small proportion of the SNPs match after sorting
    testthat::expect_lte(sum(dat$SNP==dat_sorted$SNP)/nrow(dat), 0.05)
    
    #### Return command ####
    cmd <- echotabix::sort_coordinates(target_path=tmp,
                                       chrom_col = "CHR",
                                       start_col = "BP", 
                                       outputs = "command")
    testthat::expect_true(methods::is(cmd,"character"))
    
    #### Run command and save ####
    save_path <- tempfile(fileext = "2.tsv")
    save_path_out <- echotabix::sort_coordinates(target_path=tmp,
                                        chrom_col = "CHR",
                                        start_col = "BP",
                                        save_path = save_path, 
                                        outputs = "path")
    dat2 <- data.table::fread(save_path_out, nThread = 1)
    testthat::expect_equal(dat_sorted, dat2)
    
    #### Run command ####
    save_path3 <- tempfile(fileext = "3.tsv")
    dat3 <- echotabix::sort_coordinates(target_path=tmp,
                                       chrom_col = "CHR",
                                       start_col = "BP", 
                                       save_path = save_path3,
                                       outputs = "data")
    testthat::expect_equal(dat_sorted, dat3)
    
    #### Run command with chr prefix #### 
    dat$CHR <- paste0("chr",dat$CHR)
    data.table::fwrite(dat, tmp, sep="\t")
    dat4 <- echotabix::sort_coordinates(target_path=tmp,
                                        chrom_col = "CHR",
                                        start_col = "BP", 
                                        outputs = "data")
    testthat::expect_equal(dat_sorted, dat4)
    
    #### Run command with chr prefix: supply save_path #### 
    tmp3 <- tempfile(fileext = "3.tsv")
    data.table::fwrite(dat, tmp)
    sorted_path <- tempfile(fileext = "_sorted.tsv")
    out5 <- echotabix::sort_coordinates(target_path=tmp,
                                        chrom_col = "CHR",
                                        start_col = "BP", 
                                        save_path = sorted_path)
    testthat::expect_equal(dat_sorted, out5$data)
    dat5_saved <- data.table::fread(out5$path)
    testthat::expect_equal(dat_sorted, dat5_saved)
})
