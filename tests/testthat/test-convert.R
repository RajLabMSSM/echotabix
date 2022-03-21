test_that("convert works", {
  
    run_tests <- function(dat,  
                          method=eval(formals(echotabix::convert)$method)
                          ){
        tmp <- tempfile()
        data.table::fwrite(dat, tmp, sep="\t")
        #### Sorted data ####
        dat_sorted <- data.table::copy(dat)
        data.table::setkey(dat_sorted, CHR, POS)
        data.table::setkey(dat_sorted, NULL)
        
        tabix_files <- echotabix::convert(fullSS_path = tmp, 
                                          method = method) ## <- main func
        testthat::expect_true(file.exists(tabix_files$data))
        testthat::expect_true(file.exists(tabix_files$index))
         
        dat2 <- echotabix::read_bgz(tabix_files$data, 
                                    nrows = 1000)
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
    
    run_tests_method <- function(method){
      #### Locus subset ####
      dat_locus <- echodata::BST1[1:200,]
      dat2_locus <- run_tests(dat = dat_locus,
                              method = method) 
      
      #### fullSS ####
      fullSS_path <- echodata::example_fullSS()
      dat_all <- data.table::fread(fullSS_path)
      data.table::setnames(dat_all,"BP","POS")
      dat2_all <- run_tests(dat = dat_all,
                            method = method)
      
      #### fullSS with "chr" prefix ####
      dat_all[,CHR:=paste0("chr",CHR)]
      dat3_all <- run_tests(dat = dat_all,
                            method = method)
      ### Cleanup ####
      file.remove(list.files(tempdir(), full.names = TRUE, recursive = TRUE))
    }
    
    #### ---- method combo 1 ---- #####
    run_tests_method(method = list(sort_coordinates="bash", 
                                   run_bgzip="Rsamtools",
                                   index="Rsamtools"))
    
    #### ---- method combo 2 ---- #####
    run_tests_method(method = list(sort_coordinates="data.table", 
                                   run_bgzip="Rsamtools",
                                   index="Rsamtools"))
    
    #### ---- method combo 3 ---- #####
    ## Failing due to permissions error:
    ## "sh: ..._sorted.tsv: Permission denied"
    testthat::expect_failure({
      run_tests_method(method = list(sort_coordinates="bash", 
                                     run_bgzip="conda",
                                     index="Rsamtools"))
    })
    #### ---- method combo 4 ---- ##### 
    run_tests_method(method = list(sort_coordinates="bash", 
                                   run_bgzip="Rsamtools",
                                   index="conda")) 
    
    #### ---- method combo 5 ---- ##### 
    run_tests_method(method = list(sort_coordinates="bash", 
                                   run_bgzip="Rsamtools",
                                   index="seqminer")) 
    
    #### ---- method combo 5 ---- ##### 
    run_tests_method(method = list(sort_coordinates="data.table", 
                                   run_bgzip="Rsamtools",
                                   index="seqminer")) 
})
