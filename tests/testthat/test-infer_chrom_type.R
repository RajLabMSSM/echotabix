test_that("infer_chrom_type works", {
  
    dat <- echodata::BST1
    #### From data.frame ####
    has_chr <- infer_chrom_type(chrom = dat$CHR)
    testthat::expect_false(has_chr)
    
    #### From saved ####    
    tmp <- tempfile()
    dat$CHR <- paste0("chr",dat$CHR)
    data.table::fwrite(dat, tmp)
    has_chr <- infer_chrom_type(path = tmp)
    testthat::expect_true(has_chr) 
})
