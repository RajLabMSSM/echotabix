test_that("convert works", {
  
    run_tests <- function(dat,  
                          convert_methods=eval(formals(echotabix::convert)$convert_methods)
                          ){
        tmp <- tempfile()
        data.table::fwrite(dat, tmp, sep="\t")
        #### Sorted data ####
        dat_sorted <- data.table::copy(dat)
        data.table::setkey(dat_sorted, CHR, POS)
        data.table::setkey(dat_sorted, NULL)
        
        tabix_files <- echotabix::convert(target_path = tmp, 
                                          convert_methods = convert_methods) ## <- main func
        testthat::expect_true(file.exists(tabix_files$path))
        testthat::expect_true(file.exists(tabix_files$index))
         
        dat2 <- echotabix::read_bgz(tabix_files$path, 
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
    
    run_tests_method <- function(convert_methods){
      #### Locus subset ####
      dat_locus <- echodata::BST1[1:200,]
      dat2_locus <- run_tests(dat = dat_locus,
                              convert_methods = convert_methods) 
      
      #### fullSS ####
      target_path <- echodata::example_fullSS()
      dat_all <- data.table::fread(target_path)
      data.table::setnames(dat_all,"BP","POS")
      dat2_all <- run_tests(dat = dat_all,
                            convert_methods = convert_methods)
      
      #### fullSS with "chr" prefix ####
      dat_all[,CHR:=paste0("chr",CHR)]
      dat3_all <- run_tests(dat = dat_all,
                            convert_methods = convert_methods)
      ### Cleanup ####
      file.remove(list.files(tempdir(), full.names = TRUE, recursive = TRUE))
    }
    
    #### ---- convert_methods combo 1 ---- #####
    run_tests_method(convert_methods = list(sort_coordinates="bash", 
                                   run_bgzip="Rsamtools",
                                   index="Rsamtools"))
    
    #### ---- convert_methods combo 2 ---- #####
    run_tests_method(convert_methods = list(sort_coordinates="data.table", 
                                   run_bgzip="Rsamtools",
                                   index="Rsamtools"))
    
    #### ---- convert_methods combo 3 ---- #####  
    run_tests_method(convert_methods = list(sort_coordinates="bash", 
                                   run_bgzip="conda",
                                   index="Rsamtools")) 
    
    #### ---- convert_methods combo 4 ---- ##### 
    run_tests_method(convert_methods = list(sort_coordinates="bash", 
                                   run_bgzip="Rsamtools",
                                   index="conda")) 
    
    #### ---- convert_methods combo 5 ---- ##### 
    run_tests_method(convert_methods = list(sort_coordinates="bash", 
                                   run_bgzip="Rsamtools",
                                   index="seqminer")) 
    
    #### ---- convert_methods combo 6 ---- ##### 
    run_tests_method(convert_methods = list(sort_coordinates="data.table", 
                                   run_bgzip="Rsamtools",
                                   index="seqminer")) 
    
    
    #### VCF format ####
    target_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                               package = "echodata")
    tabix_files <- echotabix::convert(target_path = target_path,
                                      chrom_col = "#CHROM")
    testthat::expect_true(all(file.exists(unlist(tabix_files))))
    vcf <- VariantAnnotation::readVcf(tabix_files$path)
    testthat::expect_true(methods::is(vcf,"VCF"))
    testthat::expect_equal(dim(vcf),c(49,378))
})
