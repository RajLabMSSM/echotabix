test_that("query_vcf works", {
    
    query_dat <- echodata::BST1
    target_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
        package = "echodata"
    )
    
    local_tests <- function(vcf){
        testthat::expect_true(methods::is(vcf, "CollapsedVCF"))
        testthat::expect_equal(nrow(vcf), 49)
    }

    #### Import ####
    vcf <- echotabix::query_vcf(
        query_granges = query_dat,
        target_path = target_path, 
    )
    local_tests(vcf = vcf)
    #### Import saved subset ####
    vcf2 <- echotabix::query_vcf(
        query_granges = query_dat,
        target_path = target_path, 
    )
    local_tests(vcf = vcf2) 
    
    #### Query remote #### 
    remote_tests <- function(dat,
                             vcf_dt,
                             samples){
        #### Check data type ####
        testthat::expect_true(methods::is(vcf_dt,"data.table"))
        #### Check the dimensions ####
        testthat::expect_equal(dim(vcf_dt), c(24376,35))
        #### Check SNP overlap ####
        ## Captures 98% of requested SNPs
        testthat::expect_gte(sum(dat$SNP %in% vcf_dt$SNP)/nrow(dat), 0.98)
        #### Check that all samples in colnames ####
        sample_colnames <- sapply(samples, 
                                  function(x){sum(endsWith(colnames(vcf_dt), x))})
        testthat::expect_true(all(sample_colnames==2))
        #### Check that samples that exist (but were not requested) are NOT in the data ####
        nonsample_colnames <- sapply(c("HG00103","HG00104"), 
                                     function(x){sum(endsWith(colnames(vcf_dt), x))})
        testthat::expect_true(all(nonsample_colnames==0))
    }
    
  
    #### Get some sample names ####
    ## Fewer samples will speed up queries substantially (15-30s vs. >1.2min).
    ##
    ## Don't actually include echoLD in echotabix code, 
    ## because that would create a bidirectional dependency conflict
    ## (since  echoLD Depends on echotabix already).
    # samples <- echoLD::popDat_1KGphase1$sample[1:5] 
    samples <- c("HG00097","HG00099","HG00100","HG00101","HG00102")
    target_path <- paste(
        "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/",
        "ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz",
        sep="/")
    query_dat2 <- echodata::BST1[1:50,]
    
    #### method: conda ####
    vcf_dt1 <- echotabix::query_vcf(target_path = target_path,
                                   query_granges = query_dat2, 
                                   samples = samples, 
                                   query_save = TRUE,
                                   as_datatable = TRUE, 
                                   force_new = TRUE, 
                                   method = "conda")
    remote_tests(dat = query_dat2, 
                 vcf_dt = vcf_dt1,
                 samples = samples)
    
    #### method: variantannotation ####
    vcf_dt2 <- echotabix::query_vcf(target_path = target_path,
                                    query_granges = query_dat2, 
                                    samples = samples, 
                                    query_save = TRUE,
                                    as_datatable = TRUE, 
                                    force_new = TRUE, 
                                    method = "variantannotation")
    ## Only works with Rhtslib >=1.99.2 (and Bioc >=3.16)
    remote_tests(dat = query_dat2, 
                 vcf_dt = vcf_dt2,
                 samples = samples)
})
