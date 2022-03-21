test_that("query_table works", {
    
    query_dat <- echodata::BST1

    #### --- LOCAL FILE --- ####
    target_path <- echodata::example_fullSS()
    tabix_files <- echotabix::convert(target_path = target_path,
                                      start_col = "BP")
    ##### seqminer ####
    tab1 <- echotabix::query_table(
        target_path = tabix_files$path,
        query_dat = query_dat,
        method = "seqminer"
    ) 
    ## Check for appropriate range
    testthat::expect_true((nrow(tab1)>=6000) & (nrow(tab1) < 7000))
    ## Check that header isn't empty 
    testthat::expect_false(all(startsWith(colnames(tab1),"V")))
    
    tab1_small <- echotabix::query_table(
        target_path = tabix_files$path, 
        query_granges = construct_query(  
          query_chrom = query_dat$CHR[1],
          query_start_pos = min(query_dat$POS),
          query_end_pos =  min(query_dat$POS)+1000,
          ),  
        method = "seqminer"
    )
    ## Check for appropriate range
    testthat::expect_true((nrow(tab1_small)>=2) & (nrow(tab1_small) <= 5))
    
    ##### rsamtools #### 
    tab2 <- echotabix::query_table(
       target_path = tabix_files$path,
       query_dat = query_dat,
       method = "rsamtools"
     )  
    ## Check for appropriate range
    testthat::expect_true((nrow(tab2)>=6000) & (nrow(tab2) < 7000))
    ## Check that header isn't empty 
    testthat::expect_false(all(startsWith(colnames(tab2),"V"))) 
      
   tab2_small <- echotabix::query_table(
     target_path = tabix_files$path,
     query_granges = construct_query(  
       query_chrom = query_dat$CHR[1],
       query_start_pos = min(query_dat$POS),
       query_end_pos =  min(query_dat$POS)+1000,
     ),  
     method = "rsamtools"
   )
   ## Check for appropriate range
   testthat::expect_true((nrow(tab2_small)>=2) & (nrow(tab2_small) <= 5))
    
   ##### conda ####
   tab3 <- echotabix::query_table(
     target_path = tabix_files$path,
     query_dat = query_dat, 
     method = "conda"
   ) 
   ## Check for appropriate range
   testthat::expect_true((nrow(tab3)>=6000) & (nrow(tab3) < 7000))
   ## Check that header isn't empty 
   testthat::expect_false(all(startsWith(colnames(tab3),"V")))
   
   tab3_small <- echotabix::query_table(
     target_path = tabix_files$path,
     query_granges = construct_query(  
       query_chrom = query_dat$CHR[1],
       query_start_pos = min(query_dat$POS),
       query_end_pos =  min(query_dat$POS)+1000,
     ),  
     method = "conda"
   )
   ## Check for appropriate range
   testthat::expect_true((nrow(tab1_small)>=2) & (nrow(tab1_small) <= 5))
     
    
    #### --- REMOTE --- ####
    target_path <- file.path(
        "https://egg2.wustl.edu/roadmap/data/byFileType",
        "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
        "E099_15_coreMarks_dense.bed.bgz"
    )
    
    #### seqminer ####
    ## seqminer for some reason cannot handle remote files 
    ## No response from maintainers yet:
    ## https://github.com/zhanxw/seqminer/issues/20 
    ##
    ## added handler to switch to Rsamtools 
     tab1r <- echotabix::query_table(
       target_path = target_path,
       query_granges = construct_query(  
         query_chrom = query_dat$CHR[1],
         query_start_pos = min(query_dat$POS),
         query_end_pos =  min(query_dat$POS)+10,
       ),  
       method = "seqminer") 
   ## Check for appropriate range
   testthat::expect_equal(nrow(tab1r), 1) 
    
    #### rsamtools #### 
    tab2r <- echotabix::query_table(
        target_path = target_path,
        query_dat = query_dat,
        method = "rsamtools"
    ) 
    ## Check for appropriate range
    testthat::expect_true((nrow(tab2r)>=170) & (nrow(tab2r) <= 200))
    #### rsamtools: small #### 
    tab3r <- echotabix::query_table(
      target_path = target_path,
      query_granges = construct_query(  
        query_chrom = query_dat$CHR[1],
        query_start_pos = min(query_dat$POS),
        query_end_pos =  min(query_dat$POS)+10,
      ),  
      method = "rsamtools"
    ) 
    ## Check for appropriate range
    testthat::expect_equal(nrow(tab3r), 1)
})
