test_that("convert works", {
  
    run_tests <- function(dat){
        tmp <- tempfile()
        data.table::fwrite(dat, tmp, sep="\t")
        #### Sorted data ####
        dat_sorted <- data.table::copy(dat)
        data.table::setkey(dat_sorted, CHR, POS)
        data.table::setkey(dat_sorted, NULL)
        
        tabix_files <- echotabix::convert(fullSS_path = tmp) ## <- main func
        testthat::expect_true(file.exists(tabix_files$data))
        testthat::expect_true(file.exists(tabix_files$index))
         
        dat2 <- echodata::read_bgz(tabix_files$data, nrows = 1000)
        #### Return to normal for comparison ####
        if(grepl("chr",dat_sorted$CHR[1])) {
          dat_sorted[,CHR:=as.integer(gsub("chr","",CHR))]
          data.table::setkey(dat_sorted, CHR, POS)
          data.table::setkey(dat_sorted, NULL)
        }
        testthat::expect_equal(head(dat_sorted,1000), dat2)
        ### Clean up ####
        file.remove(unlist(tabix_files))
        file.remove(tmp)
        return(dat2)
    }
    
    #### Locus subset ####
    dat_locus <- echodata::BST1[1:200,]
    dat2_locus <- run_tests(dat = dat_locus) 
    
    #### fullSS ####
    fullSS_path <- echodata::example_fullSS()
    dat_all <- data.table::fread(fullSS_path)
    data.table::setnames(dat_all,"BP","POS")
    dat2_all <- run_tests(dat = dat_all)
    
    #### fullSS with "chr" prefix ####
    dat_all[,CHR:=paste0("chr",CHR)]
    dat3_all <- run_tests(dat = dat_all)
   
})
