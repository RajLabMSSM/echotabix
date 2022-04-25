test_that("construct_vcf_path works", {
    
    #### Setup ####
    target_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                               package = "echodata")
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    query_dat <- echodata::BST1
    query_granges <- echotabix::construct_query(query_dat = query_dat)
    
    #### Tests ####
    save_path <- echotabix::construct_vcf_path(query_granges = query_granges,
                                               locus_dir = locus_dir,
                                               target_path = target_path, 
                                               use_coord_prefix = TRUE)
    testthat::expect_equal(
        save_path,
        "results/GWAS/Nalls23andMe_2019/BST1/VCF/BST1.chr4-14737349-16737284.BST1.1KGphase3.vcf.bgz"
    )
    
    
    #### Without coordinates ####
    save_path2 <- echotabix::construct_vcf_path(query_granges = query_granges,
                                                locus_dir = locus_dir,
                                                target_path = target_path, 
                                                use_coord_prefix = FALSE)
    testthat::expect_equal(
        save_path2,
        "results/GWAS/Nalls23andMe_2019/BST1/VCF/BST1..BST1.1KGphase3.vcf.bgz"
    )
    
    
    #### Whole VCF ####
    save_path3 <- echotabix:: construct_vcf_path(query_granges = query_granges,
                                                 locus_dir = locus_dir,
                                                 target_path = target_path, 
                                                 whole_vcf = TRUE)
    testthat::expect_equal(
        save_path3,
        "results/GWAS/Nalls23andMe_2019/BST1/VCF/BST1.1KGphase3.chr4.vcf.bgz"
    )
})
