test_that("run_gunzip works", { 
    
    dat <- echodata::BST1
    tmp_gz <- tempfile(fileext = ".csv.gz")
    data.table::fwrite(dat, tmp_gz)
    
    #### conda #### 
    out1 <- echotabix::run_gunzip(path=tmp_gz, 
                                  method = "conda")
    testthat::expect_true(methods::is(out1$command,"character"))
    testthat::expect_equal(dat, out1$data)
    data1 <- data.table::fread(out1$path)
    testthat::expect_equal(dat, data1)
    
    #### Run cmd ####
    out2 <- echotabix::run_gunzip(path=tmp_gz,
                                  method = "R.utils")
    testthat::expect_true(methods::is(out2$command,"character"))
    testthat::expect_equal(dat, out2$data)
    data2 <- data.table::fread(out2$path)
    testthat::expect_equal(dat, data2)
})
