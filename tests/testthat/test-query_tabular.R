test_that("query_tabular works", {
    
    BST1 <- echodata::BST1

    #### local ####
    fullSS_path <- echodata::example_fullSS()
    fullSS_tabix <- convert(fullSS_path = fullSS_path,
                            start_col = "BP")
    tab1 <- query_tabular(
        fullSS_tabix = fullSS_tabix,
        chrom = BST1$CHR[1],
        start_pos = min(BST1$POS),
        end_pos = max(BST1$POS)
    )
    testthat::expect_gte(nrow(tab1),6000)
    
    tab1_small <- query_tabular(
        fullSS_tabix = fullSS_tabix,
        chrom = BST1$CHR[1],
        start_pos = min(BST1$POS),
        end_pos = min(BST1$POS)+1000
    )
    testthat::expect_lte(nrow(tab1_small),5)
    

    #### remote ####
    fullSS_tabix <- file.path(
        "https://egg2.wustl.edu/roadmap/data/byFileType",
        "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
        "E099_15_coreMarks_dense.bed.bgz"
    )
    if(echotabix:::get_os()=="Windows"){
        testthat::expect_error(
            tab2 <- query_tabular(
                fullSS_tabix = fullSS_tabix,
                chrom = BST1$CHR[1],
                start_pos = min(BST1$POS),
                end_pos = min(BST1$POS)+10
            )
        )
    } else {
        tab2 <- query_tabular(
            fullSS_tabix = fullSS_tabix,
            chrom = BST1$CHR[1],
            start_pos = min(BST1$POS),
            end_pos = min(BST1$POS)+10
        )
        # Seems to be downloading the entire file
        # instead of just the region requested. Something specific to Roadmap?
        testthat::expect_gte(nrow(tab2), 265000)
    }
})
