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
#' dat <- echodata::BST1
#'
#' #### local ####
#' fullSS_path <- echodata::example_fullSS()
#' tabix_files <- echotabix::convert(fullSS_path = fullSS_path, 
#'                                   start_col = "BP")
#' query_res <- echotabix::query(
#'     target_path = tabix_files$data,
#'     query_chrom = dat$CHR[1],
#'     query_start_pos = min(dat$POS),
#'     query_end_pos = max(dat$POS))
query <- function(## Target args
                  target_path,
                  target_format = NULL, 
                  
                  ## Query args
                  #### Parameters - Set 1
                  query_dat = NULL,  
                  query_chrom_col="CHR",
                  query_start_col="POS",
                  query_end_col=query_start_col,
                  query_snp_col="SNP", 
                  #### Parameters - Set 2
                  query_chrom = NULL,
                  query_start_pos=NULL,
                  query_end_pos=query_start_pos,
                  #### Extra Parameters
                  samples = character(),
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
    ## Liftover requires a data.table/GRanges object
    ## Create one here when query_dat not available.
    if(is.null(query_dat)){
        messager("Perparing dat for liftover.",v=verbose)
        dat <- construct_query(## Set 1
                               query_chrom=query_chrom, 
                               query_start_pos=query_start_pos,
                               query_end_pos=query_end_pos,
                               verbose = verbose)
    } else {dat <- query_dat}
    ## If builds are identical, will simply return query_dat
    query_dat <- liftover(dat = dat, 
                          query_genome = query_genome,
                          target_genome = target_genome, 
                          query_chrom_col = query_chrom_col, 
                          query_start_col = query_start_col,
                          query_end_col = query_end_col, 
                          style = "UCSC",
                          verbose = verbose) 
    #### VCF ####
    if(target_format=="vcf"){ 
        query_res <- query_vcf(
            target_path = target_path,
            query_dat = query_dat, 
            query_chrom_col = query_chrom_col,
            query_start_col = query_start_col,
            query_end_col = query_end_col,
            target_genome = target_genome,
            samples = samples, 
            overlapping_only = FALSE,
            query_snp_col=query_snp_col,
            query_save = query_save,
            locus_dir = locus_dir,
            save_path = vcf_path(query_dat = query_dat,
                                 target_path = target_path,
                                 query_start_col = query_start_col,
                                 query_end_col = query_end_col,
                                 locus_dir = locus_dir), 
            force_new = query_force_new,
            as_datatable = FALSE,
            verbose = verbose) 
    
    #### Table ####
    } else {  
        query_res <- query_table(
            target_path=target_path,
            #### Set 1
            query_chrom=query_chrom,
            query_start_pos=query_start_pos,
            query_end_pos=query_end_pos, 
            #### Set 2
            query_dat=query_dat,
            query_chrom_col = query_chrom_col,
            query_start_col = query_start_col,
            query_end_col = query_end_col,
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
