test_that("vcf_to_dt works", {

    vcf_file <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                            package = "echodata")
    vcf <- VariantAnnotation::readVcf(file = vcf_file)
    vcf_dt <-  echotabix::vcf_to_dt(vcf = vcf)

    testthat::expect_equal(nrow(vcf_dt), nrow(vcf) + 1L) ## header row
    testthat::expect_gte(ncol(vcf_dt), 10)
})
