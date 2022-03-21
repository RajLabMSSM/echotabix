test_that("query_vcf works", {
    
    query_dat <- echodata::BST1
    target_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
        package = "echotabix"
    )

    #### Import ####
    vcf <- echotabix::query_vcf(
        query_dat = query_dat,
        target_path = target_path, 
    )
    testthat::expect_true(methods::is(vcf, "CollapsedVCF"))
    testthat::expect_equal(nrow(vcf), 49)


    #### Import saved subset ####
    vcf2 <- echotabix::query_vcf(
        query_dat = query_dat,
        target_path = target_path, 
    )
    testthat::expect_true(methods::is(vcf2, "CollapsedVCF"))
    testthat::expect_equal(nrow(vcf), 49)
    
    
    #### Query remote #### 
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
    dat2 <- echodata::BST1[1:50,]
    vcf_dt <- echotabix:: query_vcf(target_path = target_path,
                                   query_dat = dat2, 
                                   samples = samples, 
                                   query_save = TRUE,
                                   as_datatable = TRUE, 
                                   force_new = TRUE)
    #### Check data type ####
    testthat::expect_true(methods::is(vcf_dt,"data.table"))
    #### Check the dimensions ####
    testthat::expect_equal(dim(vcf_dt), c(24376,44))
    #### Check SNP overlap ####
    ## Captures 98% of requested SNPs
    testthat::expect_gte(sum(dat2$SNP %in% vcf_dt$SNP)/nrow(dat2), 0.98)
    #### Check that all samples in colnames ####
    sample_colnames <- sapply(samples, 
                              function(x){sum(endsWith(colnames(vcf_dt), x))})
    testthat::expect_true(all(sample_colnames==3))
    #### Check that samples that exist (but were not requested) are NOT in the data ####
    nonsample_colnames <- sapply(c("HG00103","HG00104"), 
                              function(x){sum(endsWith(colnames(vcf_dt), x))})
    testthat::expect_true(all(nonsample_colnames==0))
})
