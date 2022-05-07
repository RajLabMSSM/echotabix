test_that("scanTabix_to_dt works", {
  
    fl <- system.file("extdata", "example.gtf.gz", 
                      package="Rsamtools",
                      mustWork=TRUE)
    tbx <- Rsamtools::TabixFile(fl)

    param <- GenomicRanges::GRanges(
        c("chr1", "chr2"),
        IRanges::IRanges(c(1, 1), width=100000))
    
    #### Single query ####
    queries <- Rsamtools::scanTabix(tbx, param=param[1,])
    header <- Rsamtools::headerTabix(fl) 
    query_dt <-  echotabix::scanTabix_to_dt(header = header,
                                            queries = queries)
    testthat::expect_equal(nrow(query_dt), 157)
    testthat::expect_equal(colnames(query_dt), 
                           c("query", paste0("V",seq_len(ncol(query_dt)-1)))
                           )
    testthat::expect_equal(length(unique(query_dt$query)), length(param[1,]))
    
    #### Multiple queries ####
    queries <- Rsamtools::scanTabix(tbx, param=param)
    header <- Rsamtools::headerTabix(fl) 
    query_dt <- echotabix::scanTabix_to_dt(header = header,
                                           queries = queries)
    testthat::expect_equal(nrow(query_dt), 172)
    testthat::expect_equal(colnames(query_dt), 
                           c("query", paste0("V",seq_len(ncol(query_dt)-1)))
    )
    testthat::expect_equal(length(unique(query_dt$query)), length(param))
    
    #### Multiple queries: with colnames ####
    target_path <- echodata::example_fullSS()
    tabix_files <- echotabix::convert(target_path = target_path, 
                                      start_col = "BP", 
                                      convert_methods = list(
                                        run_bgzip="Rsamtools",
                                        index="seqminer"), 
                                      force_new = TRUE)
    query_dat <- echodata::BST1
    gr <-  echotabix::construct_query(query_dat = query_dat, 
                                       query_start_col = "POS")
    
    queries <- Rsamtools::scanTabix(tabix_files$path, param=gr)
    header <- Rsamtools::headerTabix(tabix_files$path) 
    query_dt <- echotabix::scanTabix_to_dt(header = header,
                                           queries = queries, 
                                           add_query_names = FALSE)
    fullSS <- data.table::fread(target_path)
    ## Unique RSIDS are not totally overlapping in each dataset
    ## (the original data, and the query results). This is due to
    ## slightly different filtering strategies between versions of the PD GWAS
    ## summary stats when I created `echodata::BST1` vs. `echodata::example_fullSS`
    ##
    ## In other words, `echotabix` is working perfectly, 
    ## I just need to harmonise my echodata example data a bit.
    snp_intersect.fullSS <- length(intersect(fullSS$SNP, query_dat$SNP))
    snp_intersect.query_dt <- length(intersect(query_dt$SNP, query_dat$SNP))
    testthat::expect_equal(snp_intersect.fullSS, snp_intersect.query_dt)
})
