test_that("query_table works", {

    query_dat <- echodata::BST1
    ## Detect the position column name (POS or BP depending on echodata version)
    pos_col <- intersect(c("POS","BP"), colnames(query_dat))[1]
    if(is.na(pos_col)) testthat::skip("No position column (POS/BP) found in BST1")

    #### --- LOCAL FILE --- ####
    target_path <- echodata::example_fullSS()
    ## Detect position column in the fullSS file too
    fullSS_cols <- colnames(data.table::fread(target_path, nrows = 0))
    fullSS_pos <- intersect(c("BP","POS"), fullSS_cols)[1]
    if(is.na(fullSS_pos)) testthat::skip("No position column in fullSS file")
    tabix_files <- echotabix::convert(target_path = target_path,
                                      start_col = fullSS_pos)

    ##### seqminer ####
    if(requireNamespace("seqminer", quietly = TRUE)){
        tab1 <- echotabix::query_table(
            target_path = tabix_files$path,
            query_granges = query_dat,
            query_method = "seqminer"
        )
        ## Check for appropriate range
        testthat::expect_true((nrow(tab1)>=6000) & (nrow(tab1) < 7000))
        ## Check that header isn't empty
        testthat::expect_false(all(startsWith(colnames(tab1),"V")))

        tab1_small <- echotabix::query_table(
            target_path = tabix_files$path,
            query_granges = echotabix::construct_query(
              query_chrom = as.integer(query_dat$CHR[1]),
              query_start_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE)),
              query_end_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE) + 1000),
              ),
            query_method = "seqminer"
        )
        ## Check for appropriate range
        testthat::expect_true((nrow(tab1_small)>=2) & (nrow(tab1_small) <= 5))
    }

    ##### rsamtools ####
    testthat::skip_if_not_installed("Rsamtools")
    tab2 <- echotabix::query_table(
       target_path = tabix_files$path,
       query_granges = query_dat,
       query_method = "rsamtools"
     )
    ## Check for appropriate range
    testthat::expect_true((nrow(tab2)>=6000) & (nrow(tab2) < 7000))
    ## Check that header isn't empty
    testthat::expect_false(all(startsWith(colnames(tab2),"V")))

   tab2_small <- echotabix::query_table(
     target_path = tabix_files$path,
     query_granges = echotabix::construct_query(
       query_chrom = as.integer(query_dat$CHR[1]),
       query_start_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE)),
       query_end_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE) + 1000),
     ),
     query_method = "rsamtools"
   )
   ## Check for appropriate range
   testthat::expect_true((nrow(tab2_small)>=2) & (nrow(tab2_small) <= 5))

   ##### conda ####
   conda_available <- tryCatch(
       echoconda::env_exists(conda_env = "echoR_mini"),
       error = function(e) FALSE
   )
   if(conda_available){
       tab3 <- echotabix::query_table(
         target_path = tabix_files$path,
         query_granges = query_dat,
         query_method = "conda"
       )
       ## Check for appropriate range
       testthat::expect_true((nrow(tab3)>=6000) & (nrow(tab3) < 7000))
       ## Check that header isn't empty
       testthat::expect_false(all(startsWith(colnames(tab3),"V")))

       tab3_small <- echotabix::query_table(
         target_path = tabix_files$path,
         query_granges = echotabix::construct_query(
           query_chrom = as.integer(query_dat$CHR[1]),
           query_start_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE)),
           query_end_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE) + 1000),
         ),
         query_method = "conda"
       )
       ## Check for appropriate range
       testthat::expect_true((nrow(tab3_small)>=2) & (nrow(tab3_small) <= 5))
   }

    #### --- REMOTE --- ####
    testthat::skip_if_offline()
    testthat::skip_on_ci()
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
       query_granges = echotabix::construct_query(
         query_chrom = as.integer(query_dat$CHR[1]),
         query_start_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE)),
         query_end_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE) + 10),
       ),
       query_method = "seqminer")
   ## Check for appropriate range
   ### Failing until Rsamtools method (default) fixed
   testthat::expect_failure(
       testthat::expect_equal(nrow(tab1r), 1)
   )

    #### rsamtools ####
    tab2r <- echotabix::query_table(
        target_path = target_path,
        query_granges = query_dat,
        query_method = "rsamtools"
    )
    ## Check for appropriate range
   ### Failing atm
   testthat::expect_failure(
       testthat::expect_true((nrow(tab2r)>=170) & (nrow(tab2r) <= 200))
   )
    #### rsamtools: small ####
    tab3r <- echotabix::query_table(
      target_path = target_path,
      query_granges = echotabix::construct_query(
        query_chrom = as.integer(query_dat$CHR[1]),
        query_start_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE)),
        query_end_pos = as.integer(min(query_dat[[pos_col]], na.rm = TRUE) + 10),
      ),
      query_method = "rsamtools"
    )
    ## Check for appropriate range
    testthat::expect_failure(
        testthat::expect_equal(nrow(tab3r), 1)
    )
})
