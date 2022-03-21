#' Query tabix
#' 
#' Query a tabix table or VCF.
#' 
#' @inheritParams construct_query 
#' @inheritParams convert_and_query
#' @inheritParams query_vcf
#' @inheritParams query_table
#' @export
#' @examples 
#' query_dat <- echodata::BST1
#'
#' #### local ####
#' fullSS_path <- echodata::example_fullSS()
#' tabix_files <- echotabix::convert(fullSS_path = fullSS_path, 
#'                                   start_col = "BP")
#' query_res <- echotabix::query(
#'     target_path = tabix_files$path,
#'     query_dat = query_dat)
query <- function(## Target args
                  target_path,
                  target_format = NULL, 
                  
                  ## Query args 
                  query_dat,
                  query_granges = construct_query(  
                      query_dat=query_dat,
                      query_chrom_col="CHR",
                      query_start_col="POS",
                      query_snp_col="SNP"),
                  samples = character(),
                  #### Extra Parameters 
                  query_save = TRUE,
                  locus_dir = tempdir(),
                  query_save_path=tempfile(
                      fileext = ".gz"),
                  
                  ## Genome builds 
                  target_genome = "GRCh37", 
                  query_genome = "GRCh37",
                  
                  ## Method args 
                  query_method=c(
                      "rsamtools",
                      "seqminer", 
                      "conda"), 
                  conda_env = "echoR",
                  
                  ### Force new  
                  query_force_new = FALSE,
                  
                  ## Extra args
                  nThread = 1,
                  verbose = TRUE){
    #### Check format type ####
    target_format <- infer_tabix_format(format = target_format, 
                                        path = target_path,
                                        verbose = verbose) 
    #### Liftover ###  
    ## If builds are identical, will simply return query_dat
    query_granges <- liftover(dat = query_granges, 
                              query_genome = query_genome,
                              target_genome = target_genome, 
                              style = "NCBI",
                              as_granges = TRUE,
                              verbose = verbose) 
    #### VCF ####
    if(target_format=="vcf"){ 
        query_res <- query_vcf(
            target_path = target_path,
            target_genome = target_genome, 
            query_dat = query_dat,
            query_granges = query_granges,
            samples = samples,
            overlapping_only = FALSE, 
            query_save = query_save,
            locus_dir = locus_dir,
            save_path = vcf_path(target_path = target_path,
                                 query_granges = query_granges,
                                 locus_dir = locus_dir), 
            force_new = query_force_new,
            as_datatable = FALSE,
            verbose = verbose) 
    
    #### Table ####
    } else {  
        query_res <- query_table(
            target_path=target_path,
            query_dat = query_dat,
            query_granges = query_granges,
            method = query_method,
            local = NULL,
            query_save = query_save,
            save_path = query_save_path, 
            conda_env = conda_env,
            nThread = nThread,
            verbose = verbose)
    } 
    return(query_res)
}
