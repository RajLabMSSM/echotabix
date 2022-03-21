test_that("vcf_path works", {
    
    path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                        package = "echotabix"
    )
  
    locus_dir <- echodata::locus_dir
    dat <- echodata::BST1
    save_path <- echotabix::vcf_path(dat = dat,
                                     locus_dir = locus_dir,
                                     path = path, 
                                     use_coord_prefix = TRUE)
    testthat::expect_equal(
        save_path,
        "results/GWAS/Nalls23andMe_2019/BST1/VCF/BST1.chr4-14737349-16737284.BST1.1KGphase3.vcf.bgz"
    )
    
    
    #### Without coordinates ####
    save_path2 <- echotabix::vcf_path(dat = dat,
                                     locus_dir = locus_dir,
                                     path = path, 
                                     use_coord_prefix = FALSE)
    testthat::expect_equal(
        save_path2,
        "results/GWAS/Nalls23andMe_2019/BST1/VCF/BST1..BST1.1KGphase3.vcf.bgz"
    )
    
    
    #### Whole VCF ####
    save_path3 <- echotabix:: vcf_path(dat = dat,
                                      locus_dir = locus_dir,
                                      path = path, 
                                      whole_vcf = TRUE)
    testthat::expect_equal(
        save_path3,
        "results/GWAS/Nalls23andMe_2019/BST1/VCF/BST1.1KGphase3.chr4-14737349-16737284.vcf.bgz"
    )
})
