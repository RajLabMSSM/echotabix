test_that("dt_to_vcf works", {
  
    dat <- echodata::BST1
    save_path <- echotabix::dt_to_vcf(dat=dat, 
                                      tabix_index = TRUE)
    testthat::expect_true(echotabix:::is_tabix(save_path))
    
    vcf <- VariantAnnotation::readVcf(file = save_path)
    testthat::expect_equal(nrow(vcf), nrow(dat))
    testthat::expect_equal(ncol(vcf), 1)
})
