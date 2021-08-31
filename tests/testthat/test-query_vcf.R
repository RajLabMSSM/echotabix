test_that("query_vcf works", {
    data("BST1")
    vcf_url <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
        package = "echotabix"
    )

    #### Import ####
    vcf <- echotabix::query_vcf(
        dat = BST1,
        vcf_url = vcf_url,
        vcf_name = "1KGphase3"
    )
    testthat::expect_true(methods::is(vcf, "CollapsedVCF"))


    #### Import saved subset ####
    vcf2 <- echotabix::query_vcf(
        dat = BST1,
        vcf_url = vcf_url,
        vcf_name = "1KGphase3"
    )
    testthat::expect_true(methods::is(vcf2, "CollapsedVCF"))
})
