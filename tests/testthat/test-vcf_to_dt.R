test_that("vcf_to_dt works", {
  
    vcf_file <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                            package = "echodata") 
    vcf <- VariantAnnotation::readVcf(file = vcf_file)
    vcf_dt <- echotabix::vcf_to_dt(vcf = vcf)
    
    testthat::expect_equal(dim(vcf_dt), c(49,1161))
})
