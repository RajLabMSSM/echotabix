test_that("query_vcf_variantannotation works", {
  
    query_dat <- echodata::BST1[1:50,]
    target_path <- file.path(
        "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/",
        "ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz"
    )
    #### Get some sample names ####
    ## Fewer samples will speed up queries substantially (15-30s vs. >1.2min).
    ##
    ## Don't actually include echoLD in echotabix code, 
    ## because that would create a bidirectional dependency conflict
    ## (since  echoLD Depends on echotabix already).
    # samples <- echoLD::popDat_1KGphase1$sample[1:5]
    samples <- c("HG00097","HG00099","HG00100","HG00101","HG00102")
    
    {
      t0 <- Sys.time()
      vcf <- echotabix:::query_vcf_variantannotation(target_path = target_path,
                                                     query_granges = query_dat, 
                                                     samples = samples)
      time_variantannotation <- echotabix:::report_time(start = t0,
                                                return_time = TRUE)
    }
    testthat::expect_true(methods::is(vcf,"CollapsedVCF"))
    testthat::expect_equal(rownames(vcf@colData), samples)
    testthat::expect_failure(
        testthat::expect_equal(nrow(vcf), 24376)
    )
    
    
    # ##### Try some other methods ####
    ## SUMMARY: VariantAnnotation is the best method (when it works).
    ##
    # gr <- echotabix::construct_query(query_dat = query_dat)
    # coord_range <- echotabix::granges_to_string(gr = gr)
    #  #### Rsamtools ####
    #  ## SUMMARY: Works, but gives the results as a list of raw strings.
    # {
    #   t1 <- Sys.time()
    #   tab <- Rsamtools::scanTabix(file = target_path, param=gr)
    #   time_rsamtools <- echotabix:::report_time(start = t1,
    #                                             return_time = TRUE)
    # }
    #  #### Rtracklayer ####
    #  ## SUMMARY: Doesn't work at all
    # {
    #   testthat::expect_error({
    #     t2 <- Sys.time()
    #     tab_path <- Rsamtools::TabixFile(target_path)
    #     open(tab_path)
    #     tab <- rtracklayer::import(tab_path, which=gr) 
    #     time_rtracklayer <- echotabix:::report_time(start = t2,
    #                                                 return_time = TRUE)
    #   })
    # } 
    #  #### seqminer #### 
    #  ## SUMMARY: Slower (~1.8min), 
    #  ## but i think that's because it's doing the text parsing --> data.frame
    #  ## Perhaps also the fact that it's not subsetting by sample.
    #  {
    #    t3 <- Sys.time() 
    #    tab <- seqminer::tabix.read.table(tabixFile = target_path,
    #                                      tabixRange = coord_range)
    #    # head(tab[,1:10])
    #    time_seqminer <- echotabix:::report_time(start = t3,
    #                                                return_time = TRUE)
    #  } 
})
