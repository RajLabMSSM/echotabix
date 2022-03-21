test_that("rsamtools works", { 
    
    #### Setup ####
    
    #### Get example data (Parkinson's Disease GWAS) #### 
    target_path <- echodata::example_fullSS()
    #### Construct query as granges ####
    dat <- echodata::BST1
    gr <- GenomicRanges::GRanges(
        seqnames = as.integer(dat$CHR),
        ranges = IRanges::IRanges(
            start = as.integer(dat$POS),
            end = as.integer(dat$POS)
        )
    )
    #### For reconstructing the data as a data.table with colnames ####
    scanTabix_to_dt <- function(bgz, query){
        ### Add missing header back in 
        header <- Rsamtools::headerTabix(bgz) 
        query_dt <- data.table::fread(paste(c(header$header, query),
                                            collapse = "\n"), fill=TRUE)
        return(query_dt)
    } 
    
    
    
    #### ------- Example 1: Remote file ------- ####
    ### Currently produces error
    
    ## This is an tabix-indexed and bgzip-compressed ENCODE file.
    bgz1 <- file.path(
        "https://egg2.wustl.edu/roadmap/data/byFileType",
        "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
        "E099_15_coreMarks_dense.bed.bgz"
    ) 
    #### Query ####
    testthat::expect_error(
        query1 <- Rsamtools::scanTabix(file = bgz1, param = gr) # <-- error occurs here
    ) 
    #  "Error: scanTabix: '4' not present in tabix index"
    
    #### Running without the gr query returns the while file ####
    ## However, because it's the whole file, this takes a while.
    # query1 <- Rsamtools::scanTabix(file = bgz1)
    ## This part tends to crash R (memory overload?)
    # query_dt1 <- scanTabix_to_dt(bgz1, query1)
    
    
    
    
    #### ------- Example 2: Local file ------- #### 
    ### Currently working
    
    bgz2 <- Rsamtools::bgzip(file = target_path, 
                            dest = paste0(target_path,".bgz"), 
                            overwrite = TRUE)
    tbi2 <- Rsamtools::indexTabix(file = bgz2, 
                                 seq = 2, 
                                 start = 3,
                                 end = 3, 
                                 comment = "SNP")  
    #### Query ####
    query2 <- Rsamtools::scanTabix(file = bgz2, param = gr) 
    query_dt2 <- scanTabix_to_dt(bgz2, query2)
    testthat::expect_true(nrow(query_dt2) >= 6000 & nrow(query_dt2) <= 7000)
    
    
    
    #### ------- Example 3: Local file prepared without Rsamtools ------- #### 
    ### Currently working 
    ## Tho not when bgzipping with CLI. may be a version conflict issue
    ## (unrelated to Rsamtools per se).
    
    target_path <- echodata::example_fullSS()
    bgz3 <- paste0(target_path,".bgz") 
    #### Compress the file via CLI ####
    bgzip_method <- "rsamtools"
    if(bgzip_method == "cli"){
        system(paste("bgzip -f", target_path))
        ## However, this causes an error with tabix:
        ## "[tabix] the compression of '..._subset.tsv.bgz' is not BGZF". weird!
        #### Check version of bgzip ####
        help <- system("bgzip -h", intern = TRUE)
        cat(help, sep = "\n")
    } else {
        ## Compressing with Rsamtools seems to work fine with tabix CLI 
        bgz3 <- Rsamtools::bgzip(file = target_path,
                                 dest = bgz3, 
                                 overwrite = TRUE)
    }
    #### Tabix-index ####
    system(paste("tabix",
                 "-f",
                 "-h",
                 "-s",2,
                 "-b",3,
                 "-e",3,
                 "-c","SNP",
                 bgz3
    )) 
    query3 <- Rsamtools::scanTabix(file = bgz3, param = gr)
    query_dt3 <- scanTabix_to_dt(bgz3, query3)
    testthat::expect_true(nrow(query_dt3) >= 6000 & nrow(query_dt3) <= 7000)    
})
